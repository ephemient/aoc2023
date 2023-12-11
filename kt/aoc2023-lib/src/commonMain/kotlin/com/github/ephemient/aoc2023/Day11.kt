package com.github.ephemient.aoc2023

import kotlin.math.abs

class Day11(input: String) {
    private val rows: List<Int>
    private val cols: List<Int>
    private val galaxies: List<IntPair>

    init {
        val lines = input.lines()
        rows = lines.indices.filter { '#' !in lines[it] }
        cols = (0 until (lines.maxOfOrNull { it.length } ?: 0))
            .filter { col -> lines.all { it.getOrNull(col) != '#' } }
        galaxies = buildList {
            for ((y, row) in lines.withIndex()) {
                for ((x, char) in row.withIndex()) {
                    if (char == '#') add(IntPair(y, x))
                }
            }
        }
    }

    fun part1(): Long = solve(2)

    fun part2(): Long = solve(1000000)

    internal fun solve(n: Long): Long = galaxies.withIndex().sumOf { (i, first) ->
        val (y0, x0) = first
        galaxies.subList(i + 1, galaxies.size).sumOf { (y1, x1) ->
            y1 - y0 + abs(x1 - x0) + (n - 1L) * (
                rows.binarySearch { it - y0 } - rows.binarySearch { it - y1 } +
                    abs(cols.binarySearch { it - x0 } - cols.binarySearch { it - x1 })
                )
        }
    }
}
