package sbt.internal.io

import sbt.io.BetterPollingWatchService

class BetterPollingWatchServiceSpec extends SourceModificationWatchSpec(new BetterPollingWatchService(500))

