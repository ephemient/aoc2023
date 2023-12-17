package com.github.ephemient.aoc2023

import kotlinx.coroutines.flow.channelFlow
import kotlinx.coroutines.flow.fold
import kotlinx.coroutines.launch

class Day8(input: String) {
    private val instructions = INSTRUCTIONS.matchAt(input, 0)!!.value
    private val table = NODE.findAll(input).associateBy(
        keySelector = { it.groupValues[1] },
        valueTransform = { it.groupValues[2] to it.groupValues[3] }
    )

    private fun step(start: String) = instructions.fold(start) { acc, char ->
        when (char) {
            'L' -> table[acc]!!.first
            'R' -> table[acc]!!.second
            else ->
                @Suppress("ThrowingExceptionsWithoutMessageOrCause", "UseCheckOrError")
                throw IllegalStateException()
        }
    }

    fun part1(): Int = instructions.length * generateSequence("AAA", ::step).indexOf("ZZZ")

    suspend fun part2(): Long = instructions.length * channelFlow {
        for (start in table.keys) {
            if (!start.endsWith('A')) continue
            launch {
                val (index, end) = generateSequence(start, ::step).withIndex().first { (_, end) -> end.endsWith('Z') }
                check(step(start) == step(end)) { "required for lcm solution" }
                send(index)
            }
        }
    }.fold(1L) { x, y -> lcm(x, y.toLong()) }

    companion object {
        private val INSTRUCTIONS = """[LR]+""".toRegex()
        private val NODE = """(\w+) = \((\w+), (\w+)\)""".toRegex()
    }
}
