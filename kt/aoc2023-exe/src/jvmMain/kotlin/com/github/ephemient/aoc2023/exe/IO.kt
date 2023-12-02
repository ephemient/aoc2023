package com.github.ephemient.aoc2023.exe

import java.io.File

internal actual fun getDayInput(day: Int): String =
    File(System.getenv("AOC2023_DATADIR")?.ifEmpty { null } ?: ".", "day$day.txt").readText()
