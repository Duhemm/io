package sbt.internal.io

import sbt.io.BetterPollingService

class BetterPollingWatchServiceSpec extends SourceModificationWatchSpec(new BetterPollingWatchService(500))

