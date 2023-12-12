package com.github.ephemient.aoc2023.exe

internal expect fun getDayInput(day: Int): String

internal expect fun runSuspend(block: suspend () -> Unit): SuspendResult

@Suppress("NO_ACTUAL_FOR_EXPECT")
internal expect class SuspendResult
