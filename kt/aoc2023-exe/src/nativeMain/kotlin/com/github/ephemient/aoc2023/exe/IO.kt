package com.github.ephemient.aoc2023.exe

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toKString
import okio.Path.Companion.toPath
import okio.buffer
import platform.posix.getenv

@OptIn(ExperimentalForeignApi::class)
internal actual fun getDayInput(day: Int): String {
    val dataDir = (getenv("AOC2023_DATADIR")?.toKString()?.ifEmpty { null } ?: ".").toPath()
    return okio.FileSystem.SYSTEM.source(dataDir / "day$day.txt").buffer().readUtf8()
}
