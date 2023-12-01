package com.github.ephemient.aoc2023.exe

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import java.io.File

@Suppress("InjectDispatcher")
internal actual suspend fun getDayInput(day: Int): String = withContext(Dispatchers.IO) {
    File(System.getenv("AOC2023_DATADIR")?.ifEmpty { null } ?: ".", "day$day.txt").readText()
}
