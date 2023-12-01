package com.github.ephemient.aoc2023.exe

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toKString
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.IO
import kotlinx.coroutines.withContext
import okio.Path.Companion.toPath
import okio.buffer
import platform.posix.getenv

@OptIn(ExperimentalForeignApi::class)
internal actual suspend fun getDayInput(day: Int): String = withContext(Dispatchers.IO) {
    val dataDir = (getenv("AOC2023_DATADIR")?.toKString()?.ifEmpty { null } ?: ".").toPath()
    okio.FileSystem.SYSTEM.source(dataDir / "day$day.txt").buffer().readUtf8()
}
