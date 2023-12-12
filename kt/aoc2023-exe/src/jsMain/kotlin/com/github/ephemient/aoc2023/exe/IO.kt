package com.github.ephemient.aoc2023.exe

import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.promise
import node.buffer.BufferEncoding
import node.fs.readFileSync
import node.process.process
import kotlin.js.Promise

internal actual fun getDayInput(day: Int): String {
    val dataDir = process.env["AOC2023_DATADIR"]?.ifEmpty { null } ?: "."
    return readFileSync("$dataDir/day$day.txt", BufferEncoding.utf8)
}

internal actual fun runSuspend(block: suspend () -> Unit): SuspendResult = GlobalScope.promise {
    block()
}

@Suppress("ACTUAL_WITHOUT_EXPECT", "ACTUAL_TYPE_ALIAS_TO_CLASS_WITH_DECLARATION_SITE_VARIANCE")
internal actual typealias SuspendResult = Promise<Unit>
