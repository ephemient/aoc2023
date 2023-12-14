package com.github.ephemient.aoc2023

class Day14(input: String) {
    private val input = with(input.lines()) {
        val width = fold(0) { acc, line -> maxOf(acc, line.length) }
        mapNotNull { it.ifEmpty { null }?.padEnd(width, '.') }.ifEmpty { listOf("") }
    }

    fun part1(): Int = input.tilt().load()

    fun part2(): Int {
        var state = input
        val cache = mutableMapOf(state to 0)
        for (i in 1..N) {
            state = state.spin()
            val j = cache.getOrPut(state) { i }
            if (i != j) {
                repeat((N - i) % (i - j)) { state = state.spin() }
                break
            }
        }
        return state.load()
    }

    companion object {
        private const val N = 1000000000

        @Suppress("NestedBlockDepth")
        private fun List<String>.tilt(): List<String> = map { StringBuilder(it) }.apply {
            for (x in first().indices) {
                var y0 = 0
                while (y0 < size) {
                    var n = 0
                    var y1 = y0
                    do {
                        val c = this[y1][x]
                        if (c == 'O') n++
                    } while (c != '#' && ++y1 < size)
                    for (y in y0 until y1) this[y][x] = if (y < y0 + n) 'O' else '.'
                    y0 = y1 + 1
                }
            }
        }.map { it.toString() }

        private fun List<String>.spin(): List<String> = this
            .tilt().asReversed().transpose()
            .tilt().asReversed().transpose()
            .tilt().asReversed().transpose()
            .tilt().asReversed().transpose()

        private fun List<String>.load(): Int = foldIndexed(0) { i, acc, line ->
            acc + (size - i) * line.count { it == 'O' }
        }
    }
}
