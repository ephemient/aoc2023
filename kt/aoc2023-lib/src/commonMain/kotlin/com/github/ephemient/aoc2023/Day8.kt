package com.github.ephemient.aoc2023

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

    fun part2(): Long = instructions.length *
        table.keys.filter { it.endsWith('A') }.map { start ->
            generateSequence(start, ::step).indexOfFirst { it.endsWith('Z') }
        }.fold(1L) { x, y -> lcm(x, y.toLong()) }

    companion object {
        private val INSTRUCTIONS = """[LR]+""".toRegex()
        private val NODE = """(\w+) = \((\w+), (\w+)\)""".toRegex()
    }
}
