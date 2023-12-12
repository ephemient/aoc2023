package com.github.ephemient.aoc2023.web.worker

import com.github.ephemient.aoc2023.web.common.DefaultSolver
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import web.workers.DedicatedWorkerGlobalScope
import kotlin.js.Json

private external val self: DedicatedWorkerGlobalScope

fun main() {
    val solver = DefaultSolver

    self.onerror = { console.error(it) }
    self.onmessageerror = { console.error(it) }
    self.onmessage = {
        val data = it.data as Json
        val index = data["index"] as Int
        val part = data["part"] as Int
        val input = data["input"] as String
        GlobalScope.launch {
            val result = solver.solveDayPart(index, part, input)
            self.postMessage(result)
        }
    }
}
