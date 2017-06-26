package sbt.io

import java.nio.file.{ClosedWatchServiceException, Files, Path => JPath, Watchable, WatchKey, WatchEvent}
import java.nio.file.StandardWatchEventKinds._
import java.util.{List => JList}

import sbt.io.syntax._
import scala.collection.mutable

class BetterPollingWatchService(delayMs: Long) extends WatchService {
  private var closed: Boolean       = false
  private val thread: PollingThread = new PollingThread(delayMs)
  private val keys: mutable.Map[JPath, PollingWatchKey] = mutable.Map.empty
  private val pathLengthOrdering: Ordering[JPath] =
    Ordering.fromLessThan {
      case (null, _) | (_, null) => true
      case (a, b) =>
        a.toString.length < b.toString.length
    }

  private val watched: mutable.SortedMap[JPath, Seq[WatchEvent.Kind[JPath]]] =
    mutable.SortedMap.empty(pathLengthOrdering)

  override def close(): Unit =
    closed = true

  override def init(): Unit = {
    ensureNotClosed()
    thread.start()
    while (!thread.initDone) {
      Thread.sleep(100)
    }
  }

  override def poll(timeoutMs: Long): WatchKey = thread.keysWithEvents.synchronized {
    ensureNotClosed()
    thread.keysWithEvents.headOption.map { k =>
      thread.keysWithEvents -= k
      k
    }.orNull
  }

  override def pollEvents(): Map[WatchKey, Seq[WatchEvent[JPath]]] = thread.keysWithEvents.synchronized {
    import scala.collection.JavaConverters._
    ensureNotClosed()
    val events = thread.keysWithEvents.map { k =>
      k -> k.pollEvents().asScala.asInstanceOf[Seq[WatchEvent[JPath]]]
    }
    thread.keysWithEvents.clear()
    events.toMap
  }

  override def register(path: JPath, events: WatchEvent.Kind[JPath]*): WatchKey = {
    ensureNotClosed()
    val key = new PollingWatchKey(thread, path, new java.util.ArrayList[WatchEvent[_]])
    keys += path -> key
    watched += path -> events
    key
  }

  private def ensureNotClosed(): Unit =
    if (closed) throw new ClosedWatchServiceException

  private class PollingThread(delayMs: Long) extends Thread {
    private var fileTimes: Map[JPath, Long] = Map.empty
    var initDone = false
    val keysWithEvents = mutable.LinkedHashSet.empty[WatchKey]

    override def run(): Unit =
      while (!closed) {
        populateEvents()
        initDone = true
        Thread.sleep(delayMs)
      }

    def getFileTimes(): Map[JPath, Long] = {
      val results = mutable.Map.empty[JPath, Long]
      watched.foreach {
        case (p, _) =>
          if (!results.contains(p))
            p.toFile.allPaths.get.foreach(f => results += f.toPath -> f.lastModified)
      }
      results.toMap
    }

    private def addEvent(path: JPath, ev: WatchEvent[JPath]): Unit = keysWithEvents.synchronized {
      keys.get(path).foreach { k =>
        keysWithEvents += k
        k.events.add(ev)
      }
    }

    private def populateEvents(): Unit = {
      val newFileTimes = getFileTimes()
      val newFiles     = newFileTimes.keySet
      val oldFiles     = fileTimes.keySet

      val deletedFiles  = (oldFiles -- newFiles).toSeq
      val createdFiles  = (newFiles -- oldFiles).toSeq

      val modifiedFiles = fileTimes.collect {
        case (p, oldTime) if newFileTimes.getOrElse(p, 0L) > oldTime => p
      }
      fileTimes = newFileTimes

      deletedFiles.foreach { deleted =>
        val parent = deleted.getParent
        if (watched.getOrElse(parent, Seq.empty).contains(ENTRY_DELETE)) {
          val ev = new PollingWatchEvent(parent.relativize(deleted), ENTRY_DELETE)
          addEvent(parent, ev)
        }
        watched -= deleted
      }

      createdFiles.sorted(pathLengthOrdering).foreach {
        case dir if Files.isDirectory(dir) =>
          val parent = dir.getParent
          val parentEvents = watched.getOrElse(parent, Seq.empty)
          if (parentEvents.contains(ENTRY_CREATE)) {
            val ev = new PollingWatchEvent(parent.relativize(dir), ENTRY_CREATE)
            addEvent(parent, ev)
          }

        case file =>
          val parent = file.getParent
          if (watched.getOrElse(parent, Seq.empty).contains(ENTRY_CREATE)) {
            val ev = new PollingWatchEvent(parent.relativize(file), ENTRY_CREATE)
            addEvent(parent, ev)
          }
      }

      modifiedFiles.foreach {
        case file =>
          val parent = file.getParent
          if (watched.getOrElse(parent, Seq.empty).contains(ENTRY_MODIFY)) {
            val ev = new PollingWatchEvent(parent.relativize(file), ENTRY_MODIFY)
            addEvent(parent, ev)
          }
      }
    }

  }

  private case class PollingWatchKey(origin: PollingThread,
    override val watchable: Watchable,
    val events: JList[WatchEvent[_]]) extends WatchKey {
      override def cancel(): Unit = ()
      override def isValid(): Boolean = true
      override def pollEvents(): java.util.List[WatchEvent[_]] = origin.keysWithEvents.synchronized {
        origin.keysWithEvents -= this
        val evs = new java.util.ArrayList[WatchEvent[_]](events)
        events.clear()
        evs
      }
      override def reset(): Boolean = true
    }

}

private class PollingWatchEvent(override val context: JPath,
                                override val kind: WatchEvent.Kind[JPath]) extends WatchEvent[JPath] {
  override val count: Int = 1
}

