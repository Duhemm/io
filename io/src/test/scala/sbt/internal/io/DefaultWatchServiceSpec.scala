package sbt.internal.io

import java.nio.file.FileSystems

class DefaultWatchServiceSpec extends SourceModificationWatchSpec(FileSystems.getDefault.newWatchService)

