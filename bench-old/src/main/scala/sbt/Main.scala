package sbt

import sbt.io._, syntax._
import sbt.internal.io.{ SourceModificationWatch, WatchState }
import scala.collection.mutable.Buffer

object Main {

  def main(args: Array[String]): Unit = {
    val defaultFiles      = 1 //3
    val defaultDirs       = 1 //3
    val defaultDepth      = 1 //2
    val defaultIterations = 1 //10

    val files      = args.lift(0).map(_.toInt).getOrElse(defaultFiles)
    val dirs       = args.lift(1).map(_.toInt).getOrElse(defaultDirs)
    val depth      = args.lift(2).map(_.toInt).getOrElse(defaultDepth)
    val iterations = args.lift(3).map(_.toInt).getOrElse(defaultIterations)

    bench(files, dirs, depth, iterations)
  }

  /** Benchmarks all of `services` on a generated hierarchy. */
  private def bench(files: Int,
                    dirs: Int,
                    depth: Int,
                    iterations: Int): Unit = IO.withTemporaryDirectory { base =>

    genHierarchy(base, files, dirs, depth)

    println(s"""Results
               |=======
               | - files      = $files
               | - dirs       = $dirs
               | - depth      = $depth
               | - iterations = $iterations
               |""".stripMargin)

    val timesMs = bench(base, iterations)

    val sortedMs   = timesMs.sorted
    val minMs      = sortedMs.head
    val maxMs      = sortedMs.last
    val avgMs      = average(timesMs)
    val medianMs   = percentile(50, sortedMs)
    val p95Ms      = percentile(95, sortedMs)
    val p05Ms      = percentile(5, sortedMs)
    val stddevMs =
      Math.sqrt(timesMs.map(t => Math.pow(t - avgMs, 2) / iterations).sum)
    val avgBetweenP05AndP95 =
      average(timesMs filter (t => t >= p05Ms && t <= p95Ms))

    println(s"""old sbt file watch
               |------------------
               | - minMs      = $minMs
               | - maxMs      = $maxMs
               | - avgMs      = $avgMs
               | - medianMs   = $medianMs
               | - stddevMs   = $stddevMs
               | - p95Ms      = $p95Ms
               | - p05Ms      = $p05Ms
               | - avg        = $avgBetweenP05AndP95
               |""".stripMargin)
  }

  /**
   *  Bench `service` in `base`, running `iterations` runs.
   *  Returns the number of milliseconds elapsed to detect
   *  changes at each iteration.
   */
  private def bench(base: File, iterations: Int): Seq[Double] = {
    val pathFinder    = base.allPaths
    val modifier      = new Modifier(base, iterations)
    var state         = WatchState.empty
    val detectedTimes = Buffer.empty[Long]

    val (_, st) = SourceModificationWatch.watch(pathFinder, 100, state)(false)
    state = st

    modifier.setDaemon(true)
    modifier.start()

    while (!modifier.done) {
      val (triggered, newState) = SourceModificationWatch.watch(pathFinder, 100, state)(false)

      detectedTimes += System.currentTimeMillis()
      assert(triggered)
      state = newState
      synchronized { notify(); wait() }
    }

    detectedTimes.zip(modifier.modifiedTimes).map {
      case (d, m) => (d - m).toDouble
    }
  }

  private def genHierarchy(base: File, files: Int, dirs: Int, depth: Int): Unit =
    if (depth == 0) ()
    else {
      (1 to files).foreach(genFile(base, _))
      (1 to dirs).foreach(genDirectory(base, files, dirs, depth - 1, _))
    }

  private def genFile(base: File, n: Int): Unit = {
    val file = base / s"file-$n.scala"
    IO.touch(file)
  }

  private def genDirectory(base: File, files: Int, dirs: Int, depth: Int, n: Int): Unit = {
    val dir = base / s"directory-$n"
    IO.createDirectory(dir)
    genHierarchy(dir, files, dirs, depth)
  }

  private def percentile[T](n: Int, sortedData: Seq[T]): T = {
    val index = Math.ceil(n * sortedData.length / 100.0).toInt - 1
    sortedData(index)
  }

  private def average(data: Seq[Double]): Double = {
    val count = data.length.toDouble
    data.foldLeft(0.0) { _ + _ / count }
  }
}

/** Thread in charge of making file changes */
private class Modifier(base: File, iterations: Int) extends Thread {

  /** How many times we've made changes up to now */
  var count: Int = 0

  /** The times at which the files have been modified */
  val modifiedTimes: Buffer[Long] = Buffer.empty

  /** Are we done? */
  var done: Boolean = false

  override def run(): Unit = Main.synchronized {
    val allFiles = base.allPaths.get.filter(_.isFile).toArray
    while (count < iterations) {

      // Sleep to make sure we pick up file time changes
      // (we have 1s granularity on macOS for instance)
      Thread.sleep(2000)
      print(s"\r${count + 1} / $iterations...")

      val elem = allFiles(scala.util.Random.nextInt(allFiles.length))
      IO.touch(elem)
      modifiedTimes += System.currentTimeMillis()

      Main.notify()
      Main.wait()
      count += 1
    }
    done = true
    println(" Done!")
    Main.notify()
  }
}
