package com.github.ephemient.aoc2023.exe

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import java.io.File

internal actual fun getDayInput(day: Int): String =
    File(System.getenv("AOC2023_DATADIR")?.ifEmpty { null } ?: ".", "day$day.txt").readText()

@Suppress("InjectDispatcher")
internal actual fun runSuspend(block: suspend () -> Unit): SuspendResult = runBlocking(Dispatchers.Default) {
    block()
}

@Suppress("ACTUAL_WITHOUT_EXPECT")
internal actual typealias SuspendResult = Unit
