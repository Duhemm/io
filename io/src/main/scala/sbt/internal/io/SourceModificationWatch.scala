/* sbt
 * Copyright 2009-2015 Typesafe, Inc, Mark Harrah, and others
 */
package sbt.internal.io

import java.nio.file.{ Files, Path, WatchEvent, WatchKey }
import java.nio.file.StandardWatchEventKinds._

import sbt.io.{ DirectoryFilter, FileFilter, WatchService }
import sbt.io.syntax._

import scala.annotation.tailrec

private[sbt] object SourceModificationWatch {

  /**
   *  Checks for modifications on the file system every `delayMillis` milliseconds,
   *  until changes are detected or `terminationCondition` evaluates to `true`.
   */
  @tailrec
  def watch(delayMillis: Long, state: WatchState)(terminationCondition: => Boolean): (Boolean, WatchState) = {
    if (state.count == 0) (true, state.withCount(1))
    else {
      val events =
        state.next(delayMillis).map(expandEvent)

      val newFiles = WatchState.getPaths(state.sources).toSet
      val previousFiles = state.registered.keySet
      val hasModFiles = events.exists { case (path, kind) => newFiles.contains(path) && kind == ENTRY_MODIFY && !Files.isDirectory(path) }

      if (events.isEmpty || (previousFiles == newFiles && !hasModFiles)) {
        if (terminationCondition) {
          (false, state)
        } else {
          watch(delayMillis, state)(terminationCondition)
        }
      } else {
        val createdFiles  = newFiles -- previousFiles
        val deletedFiles  = previousFiles -- newFiles
        val newState = state.withCount(state.count + 1) ++ createdFiles -- deletedFiles
        (true, newState)
      }
    }
  }

  private def expandEvent(event: (Path, WatchEvent[_])): (Path, WatchEvent.Kind[Path]) = {
    event match {
      case (base, ev) =>
        val fullPath = base.resolve(ev.context().asInstanceOf[Path])
        val kind     = ev.kind().asInstanceOf[WatchEvent.Kind[Path]]
        (fullPath, kind)
    }
  }
}

private[sbt] final class WatchState(
  val count: Int,
  val sources: Seq[WatchState.Source],
  private val service: WatchService,
  val registered: Map[Path, WatchKey]
) {
  /** Removes all of `fs` from the watched paths. */
  def --(fs: Iterable[Path]): WatchState = {
    for { f  <- fs;
          wk <- registered.get(f);
          if (registered.values.count(_ == wk)) <= 1 } wk.cancel()
    withRegistered(registered -- fs)
  }

  /** Adds all of `fs` to the watched paths. */
  def ++(fs: Iterable[Path]): WatchState = {
    val newKeys =
      fs.filter(Files.exists(_)).foldLeft(registered) {
        case (ks, d) if Files.isDirectory(d) =>
          if (registered.contains(d)) ks
          else ks + (d -> service.register(d, WatchState.events: _*))

        case (ks, f) =>
          val parent = f.getParent
          if (registered.contains(parent)) ks + (f -> registered(parent))
          else ks + (f -> service.register(parent, WatchState.events: _*))
      }
    withRegistered(newKeys)
  }

  /**
   *  Retrieve events from the `WatchService`, waiting up to `delayMs` milliseconds
   *  if necessary.
   */
  def next(delayMs: Long): Iterable[(Path, WatchEvent[_])] = {
    val events = {
      val events = service.pollEvents()
      if (events.isEmpty) {
        Thread.sleep(delayMs)
        service.pollEvents()
      } else events
    }

    events.toIterable.flatMap {
      case (k, evs) => evs.map((k.watchable().asInstanceOf[Path], _))
    }
  }

  /** A new state, with a new `count`. */
  def withCount(count: Int): WatchState =
    new WatchState(count, sources, service, registered)

  /** A new state, with new keys registered. */
  def withRegistered(registered: Map[Path, WatchKey]): WatchState =
    new WatchState(count, sources, service, registered)
}

private[sbt] object WatchState {
  /** How to acquire a list of items to watch. */
  type Source = (File, FileFilter, FileFilter)

  /** What events should be monitored */
  val events: Array[WatchEvent.Kind[Path]] = Array(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)

  /** An empty `WatchState`. */
  def empty(service: WatchService, sources: Seq[Source]): WatchState = {
    val initFiles = getPaths(sources)
    new WatchState(0, sources, service, Map.empty) ++ initFiles
  }

  private[sbt] def getPaths(sources: Seq[Source]): Seq[Path] =
    sources.flatMap {
      case (base, includeFilter, excludeFilter) => base.descendantsExcept(includeFilter || DirectoryFilter, excludeFilter).get
    }.map(_.toPath)

}
