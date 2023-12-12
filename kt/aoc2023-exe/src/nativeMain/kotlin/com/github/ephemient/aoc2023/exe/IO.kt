package com.github.ephemient.aoc2023.exe

import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toKString
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import okio.Path.Companion.toPath
import okio.buffer
import platform.posix.getenv

@OptIn(ExperimentalForeignApi::class)
internal actual fun getDayInput(day: Int): String {
    val dataDir = (getenv("AOC2023_DATADIR")?.toKString()?.ifEmpty { null } ?: ".").toPath()
    return okio.FileSystem.SYSTEM.source(dataDir / "day$day.txt").buffer().readUtf8()
}

internal actual fun runSuspend(block: suspend () -> Unit): SuspendResult = runBlocking(Dispatchers.Default) {
    block()
}

@Suppress("ACTUAL_WITHOUT_EXPECT")
internal actual typealias SuspendResult = Unit
