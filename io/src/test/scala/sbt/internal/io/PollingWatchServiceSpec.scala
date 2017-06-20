package sbt.internal.io

import sbt.io.PollingWatchService

class PollingWatchServiceSpec extends SourceModificationWatchSpec(new PollingWatchService(500))

