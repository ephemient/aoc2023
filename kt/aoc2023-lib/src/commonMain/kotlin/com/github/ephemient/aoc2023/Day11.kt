package com.github.ephemient.aoc2023

class Day11(input: String) {
    private val lines = input.lines()

    fun part1(): Long = solve(2)

    fun part2(): Long = solve(1000000)

    internal fun solve(n: Long): Long =
        solve1(lines.map { it.count('#'::equals) }, n) + solve1(
            0.until(lines.maxOfOrNull { it.length } ?: 0).map { x ->
                lines.count { x < it.length && it[x] == '#' }
            },
            n
        )

    private fun solve1(data: List<Int>, n: Long): Long {
        var total = 0L
        for ((i, a) in data.withIndex()) {
            if (a == 0) continue
            var m = 0L
            for (j in i + 1..data.lastIndex) {
                val b = data[j]
                m += if (b == 0) n else 1
                total += m * a * b
            }
        }
        return total
    }
}
