package com.barbarysoftware.watchservice

import sbt.io
import java.io.File
import java.nio.file.{
  Path => JPath,
  Paths => JPaths,
  Watchable => JWatchable,
  WatchEvent => JWatchEvent,
  WatchKey => JWatchKey
}

import java.nio.file.StandardWatchEventKinds.{
  ENTRY_CREATE => JENTRY_CREATE,
  ENTRY_MODIFY => JENTRY_MODIFY,
  ENTRY_DELETE => JENTRY_DELETE
}

import java.util.concurrent.TimeUnit
import java.util.{ ArrayList, List }

import scala.collection.mutable.Buffer
import scala.collection.JavaConverters._

object BarbaryWatchService {
  def newWatchService: BarbaryWatchService =
    new BarbaryWatchService(WatchService.newWatchService)

  def adaptEventKind(kind: WatchEvent.Kind[_]): JWatchEvent.Kind[JPath] =
    kind match {
      case StandardWatchEventKind.ENTRY_CREATE => JENTRY_CREATE
      case StandardWatchEventKind.ENTRY_MODIFY => JENTRY_MODIFY
      case StandardWatchEventKind.ENTRY_DELETE => JENTRY_DELETE
    }

  def adaptEventKind(kind: JWatchEvent.Kind[_]): WatchEvent.Kind[WatchableFile] =
    kind match {
      case JENTRY_CREATE => StandardWatchEventKind.ENTRY_CREATE
      case JENTRY_MODIFY => StandardWatchEventKind.ENTRY_MODIFY
      case JENTRY_DELETE => StandardWatchEventKind.ENTRY_DELETE
    }
}
class BarbaryWatchService(underlying: WatchService) extends io.WatchService {
  private val registered: Buffer[WatchKey] = Buffer.empty
  private[watchservice] var watchables: Map[WatchKey, JPath] = Map.empty

  override def toString: String = underlying.toString

  override def close(): Unit = underlying.close()

  override def poll(timeoutMs: Long): JWatchKey =
    new BarbaryWatchKey(underlying.poll(timeoutMs, TimeUnit.MILLISECONDS), this)

  override def pollEvents(): Map[JWatchKey, Seq[JWatchEvent[JPath]]] = {
    registered.flatMap { k =>
      val events = k.pollEvents()
      if (events.isEmpty) None
      else {
        val key = new BarbaryWatchKey(k, this)
        val evs = events.asScala.map(e => new BarbaryWatchEvent(e.asInstanceOf[WatchEvent[JPath]]))
        Some((key, evs))
      }
    }.toMap
  }

  override def register(path: JPath, events: JWatchEvent.Kind[JPath]*): JWatchKey = {
    val watchableFile = new WatchableFile(path.toFile)
    val adaptedEvents: Array[WatchEvent.Kind[_]] = events.toArray.map(BarbaryWatchService.adaptEventKind)
    val rawKey = underlying.asInstanceOf[AbstractWatchService].register(watchableFile, adaptedEvents)
    registered += rawKey
    watchables += rawKey -> path
    new BarbaryWatchKey(rawKey, this)
  }

}

class BarbaryWatchKey(underlying: WatchKey, service: BarbaryWatchService) extends JWatchKey {
  override def toString: String = underlying.toString

  override def cancel(): Unit = underlying.cancel()

  override def isValid(): Boolean = underlying.isValid()

  override def pollEvents(): List[JWatchEvent[_]] = {
    val events = underlying.pollEvents()
    val adaptedEvents = new ArrayList[JWatchEvent[_]]
    val it = events.iterator()
    while (it.hasNext()) {
      adaptedEvents.add(new BarbaryWatchEvent(it.next()))
    }
    adaptedEvents
  }

  override def reset(): Boolean = underlying.reset()

  override def watchable(): JWatchable = service.watchables(underlying)
}

class BarbaryWatchEvent[T](underlying: WatchEvent[T]) extends JWatchEvent[JPath] {
  override def toString: String = underlying.toString
  def context(): JPath = {
    // HACK HACK HACK HACK HACK :)
    val p = underlying.context().asInstanceOf[File].toPath
    if (p.startsWith(JPaths.get("/private"))) JPaths.get("/" + p.subpath(1, p.getNameCount).toString)
    else p
  }
  def count(): Int = underlying.count()
  def kind(): JWatchEvent.Kind[JPath] = BarbaryWatchService.adaptEventKind(underlying.kind())
}
