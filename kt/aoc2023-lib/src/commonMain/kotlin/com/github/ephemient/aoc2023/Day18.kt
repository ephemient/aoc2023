package com.github.ephemient.aoc2023

import kotlin.math.abs

class Day18(private val input: String) {
    fun part1(): Long = solve(
        PATTERN_1.findAll(input).map {
            val (_, d, n) = it.groupValues
            d.single() to n.toInt()
        }
    )

    fun part2(): Long = solve(
        PATTERN_2.findAll(input).map {
            val (_, n, d) = it.groupValues
            DIRECTIONS[d.toInt()] to n.toInt(radix = 16)
        }
    )

    companion object {
        private const val DIRECTIONS = "RDLU"
        private val PATTERN_1 = """([$DIRECTIONS]) (\d+)""".toRegex()
        private val PATTERN_2 = """#([0-9a-f]{5})([0-3])""".toRegex()

        private fun solve(input: Sequence<Pair<Char, Int>>): Long {
            var y = 0L
            var x = 0L
            var a = 0L
            var l = 0L
            for ((d, n) in input) {
                when (d) {
                    'R' -> {
                        x += n
                        a += y * n
                    }
                    'D' -> y += n
                    'L' -> {
                        x -= n
                        a -= y * n
                    }
                    'U' -> y -= n
                }
                l += n
            }
            return abs(a) + l / 2 + 1
        }
    }
}
