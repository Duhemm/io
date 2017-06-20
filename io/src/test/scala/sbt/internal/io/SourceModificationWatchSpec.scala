package sbt.internal.io

import java.nio.file.{ ClosedWatchServiceException, Paths }
import java.util.concurrent.atomic.AtomicBoolean

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.syntax._
import sbt.io.{ HiddenFileFilter, IO, WatchService }

abstract class SourceModificationWatchSpec(getService: => WatchService) extends FlatSpec with Matchers {

  it should "watch a directory for file creation" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "NewSource.scala"

    IO.createDirectory(parentDir)

    watchTest(parentDir)(200L, 15000L) {
      IO.write(created, "foo")
    }
  }

  it should "watch a directory for directory creation" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "newDirectory"

    IO.createDirectory(parentDir)

    watchTest(parentDir)(200L, 15000L) {
      IO.createDirectory(created)
    }
  }

  it should "detect files created in a subdirectory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / "NewSource.scala"

    IO.createDirectory(subDir)

    watchTest(parentDir)(200L, 15000L) {
      IO.write(created, "foo")
    }
  }

  it should "detect directories created in a subdirectory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / "willBeCreated"

    IO.createDirectory(subDir)

    watchTest(parentDir)(200L, 15000L) {
      IO.createDirectory(created)
    }
  }

  it should "detect deleted files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "WillBeDeleted.scala"
    IO.write(file, "foo")

    watchTest(parentDir)(200L, 15000L) {
      IO.delete(file)
    }
  }

  it should "detect deleted directories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "willBeDeleted"
    IO.createDirectory(subDir)

    watchTest(parentDir)(200L, 15000L) {
      IO.delete(subDir)
    }
  }

  it should "detect deleted files in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / "WillBeDeleted.scala"
    IO.write(willBeDeleted, "foo")

    watchTest(parentDir)(200L, 15000L) {
      IO.delete(willBeDeleted)
    }
  }

  it should "detect deleted directories in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / "willBeDeleted"
    IO.createDirectory(willBeDeleted)

    watchTest(parentDir)(200L, 15000L) {
      IO.delete(willBeDeleted)
    }
  }

  "WatchService.poll" should "throw a `ClosedWatchServiceException` if used after `close`" in {
    val service = getService
    service.close()
    assertThrows[ClosedWatchServiceException](service.poll(1000L))
  }

  "WatchService.register" should "throw a `ClosedWatchServiceException` if used after `close`" in {
    val service = getService
    service.close()
    assertThrows[ClosedWatchServiceException](service.register(Paths.get(".")))
  }

  "WatchService.close" should "not throw if called multiple times" in {
    val service = getService
    service.close()
    service.close()
  }

  private def watchTest(base: File)(pollDelayMs: Long, maxWaitMs: Long)(modifier: => Unit) = {
    val service = getService
    try {
      val sources: Seq[WatchState.Source] = Seq((base, "*.scala", HiddenFileFilter))
      val initState = WatchState.empty(service, sources).withCount(1)
      val started = new AtomicBoolean(false)
      val startTime = System.currentTimeMillis()
      val modThread = new Thread {
        override def run(): Unit = {
          started.set(true)
          modifier
        }
      }
      val (triggered, _) = SourceModificationWatch.watch(pollDelayMs, initState) {
        if (!started.get()) modThread.start()
        System.currentTimeMillis() - startTime > maxWaitMs
      }
      triggered shouldBe true
    } finally service.close()
  }

}
