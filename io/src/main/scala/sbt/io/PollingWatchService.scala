package sbt.io

import java.nio.file.{ ClosedWatchServiceException, Files, Path => JPath, WatchEvent => JWatchEvent, WatchKey => JWatchKey }
import java.nio.file.StandardWatchEventKinds._
import java.util
import java.util.concurrent.{ LinkedBlockingQueue, TimeUnit }
import java.util.function.Consumer

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * A `WatchService` that discovers changes by regularly polling the filesystem.
 * @param delayMs The delay in milliseconds between every poll of the filesystem.
 */
class PollingWatchService(delayMs: Long) extends WatchService { self =>

  private class WatchEvent(
    override val kind: JWatchEvent.Kind[JPath],
    val key: WatchKey,
    override val context: JPath
  ) extends JWatchEvent[JPath] {
    override val count: Int = 1
  }

  private class WatchKey(path: JPath, val events: Set[JWatchEvent.Kind[JPath]]) extends JWatchKey {

    private val eventsList = new util.LinkedList[WatchEvent]()
    private var cancelled: Boolean = false

    override def cancel(): Unit = {
      cancelled = true
      keys -= this
      ()
    }

    override def pollEvents(): util.List[JWatchEvent[_]] = {
      val elements = new util.LinkedList[JWatchEvent[_]](eventsList)
      eventsList.clear()
      self.events.remove(this)
      elements
    }

    override def watchable(): JPath =
      path

    override def isValid: Boolean =
      !cancelled

    override def reset(): Boolean =
      if (isValid) {
        self.events.remove(this)
        eventsList.clear()
        true
      } else false

    private[PollingWatchService] def addEvent(event: WatchEvent): Unit = {
      eventsList.addLast(event)
      self.events.remove(this)
      self.events.put(this)
    }

  }

  private class PollingThread(delayMs: Long) extends Thread {
    private case class State(infos: Iterable[(WatchKey, JPath, Long)]) {
      lazy val files: Set[(WatchKey, JPath)] = infos.map(i => (i._1, i._2)).toSet
      lazy val times: Map[(WatchKey, JPath), Long] = infos.map { case (k, p, t) => (k, p) -> t }.toMap
    }

    private var previousState: State = State(Iterable.empty)

    override def run(): Unit =
      while (!closed) {
        findDifferences()
        Thread.sleep(delayMs)
      }

    private def collectTimes(): Iterable[(WatchKey, JPath, Long)] = {
      val buffer = mutable.Buffer.empty[(WatchKey, JPath, Long)]
      keys.foreach(collectTimes(_, buffer))
      buffer
    }

    private def collectTimes(k: WatchKey, buffer: mutable.Buffer[(WatchKey, JPath, Long)]): Unit = {
      val fn =
        new Consumer[JPath] {
          override def accept(p: JPath): Unit =
            if (Files.exists(p)) { buffer += ((k, p, Files.getLastModifiedTime(p).toMillis)); () }
        }

      if (Files.exists(k.watchable())) Files.walk(k.watchable(), 1).forEach(fn)
    }

    private def makeEvent(ev: (WatchKey, JPath), kind: JWatchEvent.Kind[JPath]): Option[WatchEvent] =
      if (ev._1.events.contains(kind)) Some(new WatchEvent(kind, ev._1, ev._2))
      else None

    private def registerEvent(ev: WatchEvent): Unit =
      ev.key.addEvent(ev)

    private def findDifferences(): Unit = {
      val newState = State(collectTimes)
      val deletedFiles = previousState.files -- newState.files
      val createdFiles = newState.files -- previousState.files
      val modifiedFiles = previousState.times.foldLeft(Map.empty[WatchKey, JPath]) {
        case (times, ((key, path), prevTime)) =>
          val newTime = newState.times.getOrElse((key, path), 0L)
          if (newTime > prevTime) times + (key -> path)
          else times
      }

      val events =
        deletedFiles.flatMap(makeEvent(_, ENTRY_DELETE)) ++
          createdFiles.flatMap(makeEvent(_, ENTRY_CREATE)) ++
          modifiedFiles.flatMap(makeEvent(_, ENTRY_MODIFY))

      events.foreach(registerEvent)

      previousState = newState
    }
  }

  private var closed: Boolean = false
  private val pollingThread = new PollingThread(delayMs)
  private val events = new LinkedBlockingQueue[WatchKey]()
  private val keys = mutable.Buffer.empty[WatchKey]

  pollingThread.setDaemon(true)
  pollingThread.start()

  override def pollEvents(): Map[JWatchKey, Seq[JWatchEvent[JPath]]] =
    keys.flatMap { k =>
      val events = k.pollEvents()
      if (events.isEmpty) None
      else Some((k, events.asScala.asInstanceOf[Seq[JWatchEvent[JPath]]]))
    }.toMap

  override def poll(timeoutMs: Long): JWatchKey = {
    ensureNotClosed()
    events.poll(timeoutMs, TimeUnit.MILLISECONDS)
  }

  override def register(path: JPath, events: JWatchEvent.Kind[JPath]*): JWatchKey = {
    ensureNotClosed()
    val key = new WatchKey(path, events.toSet)
    keys += key
    key
  }

  override def close(): Unit = {
    closed = true
  }

  private def ensureNotClosed(): Unit =
    if (closed) throw new ClosedWatchServiceException

}
