package com.github.ephemient.aoc2023.exe

import node.buffer.BufferEncoding
import node.fs.readFileSync
import node.process.process

internal actual fun getDayInput(day: Int): String {
    val dataDir = process.env["AOC2023_DATADIR"]?.ifEmpty { null } ?: "."
    return readFileSync("$dataDir/day$day.txt", BufferEncoding.utf8)
}
