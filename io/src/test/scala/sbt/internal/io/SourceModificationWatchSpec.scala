package sbt.internal.io

import java.nio.file.{ ClosedWatchServiceException, Paths }
import java.util.concurrent.atomic.AtomicBoolean

import org.scalatest.{ FlatSpec, Matchers }
import sbt.io.syntax._
import sbt.io.{ IO, SimpleFilter, WatchService }

abstract class SourceModificationWatchSpec(getService: => WatchService, pollDelayMs: Long, maxWaitMs: Long) extends FlatSpec with Matchers {

  it should "watch a directory for file creation" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "NewSource.scala"

    IO.createDirectory(parentDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs) {
      IO.write(created, "foo")
    }
  }

  it should "ignore creation of directories with no tracked sources" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "ignoreme"

    IO.createDirectory(parentDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.createDirectory(created)
    }
  }

  it should "ignore creation of files that do not match inclusion filter" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "ignoreme"

    IO.createDirectory(parentDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.touch(created)
    }
  }

  it should "ignore creation of files that are explicitly ignored" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / ".hidden.scala"

    IO.createDirectory(parentDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.touch(created)
    }
  }

  it should "ignore creation of an empty directory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val created = parentDir / "ignoreme"

    IO.createDirectory(parentDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.createDirectory(created)
    }
  }

  it should "detect files created in a subdirectory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / "NewSource.scala"

    IO.createDirectory(subDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs) {
      IO.write(created, "foo")
    }
  }

  it should "ignore creation of files not included in inclusion filter in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / "ignoreme"

    IO.createDirectory(subDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.touch(created)
    }
  }

  it should "ignore creation of files explicitly ignored in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / ".hidden.scala"

    IO.createDirectory(subDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.touch(created)
    }
  }

  it should "ignore creation of empty directories in a subdirectory" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "sub"
    val created = subDir / "ignoreme"

    IO.createDirectory(subDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.createDirectory(created)
    }
  }

  it should "detect deleted files" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "WillBeDeleted.scala"
    IO.write(file, "foo")

    watchTest(parentDir)(pollDelayMs, maxWaitMs) {
      IO.delete(file)
    }
  }

  it should "ignore deletion of files not included in inclusion filter" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / "ignoreme"
    IO.write(file, "foo")

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.delete(file)
    }
  }

  it should "ignore deletion of files explicitly ignored" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val file = parentDir / ".hidden.scala"
    IO.write(file, "foo")

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.delete(file)
    }
  }

  it should "ignore deletion of empty directories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "ignoreme"
    IO.createDirectory(subDir)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.delete(subDir)
    }
  }

  it should "detect deleted files in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / "WillBeDeleted.scala"
    IO.write(willBeDeleted, "foo")

    watchTest(parentDir)(pollDelayMs, maxWaitMs) {
      IO.delete(willBeDeleted)
    }
  }

  it should "ignore deletion of files not included in inclusion filter in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / "ignoreme"
    IO.write(willBeDeleted, "foo")

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.delete(willBeDeleted)
    }
  }

  it should "ignore deletion of files explicitly ignored in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / ".hidden.scala"
    IO.write(willBeDeleted, "foo")

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
      IO.delete(willBeDeleted)
    }
  }

  it should "ignore deletion of empty directories in subdirectories" in IO.withTemporaryDirectory { dir =>
    val parentDir = dir / "src" / "watchme"
    val subDir = parentDir / "subdir"
    val willBeDeleted = subDir / "ignoreme"
    IO.createDirectory(willBeDeleted)

    watchTest(parentDir)(pollDelayMs, maxWaitMs, expectedTrigger = false) {
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

  private def watchTest(base: File)(pollDelayMs: Long, maxWaitMs: Long, expectedTrigger: Boolean = true)(modifier: => Unit) = {
    val service = getService
    try {
      val sources: Seq[WatchState.Source] = Seq((base, "*.scala", new SimpleFilter(_.startsWith("."))))
      // Set count to 1, because first run always immediately triggers.
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
      triggered shouldBe expectedTrigger
    } finally service.close()
  }

}
