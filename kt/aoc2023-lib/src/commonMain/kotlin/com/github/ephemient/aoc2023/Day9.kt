package com.github.ephemient.aoc2023

class Day9(input: String) {
    private val nums = input.lines().mapNotNull { line ->
        line.split(' ').mapNotNull { it.toIntOrNull() }.ifEmpty { null }
    }

    fun part1(): Long = nums.sumOf { it.extrapolate() }

    fun part2(): Long = nums.sumOf { it.asReversed().extrapolate() }

    companion object {
        private fun List<Int>.extrapolate(): Long {
            var c = 1L
            var s = 0L
            for ((i, x) in withIndex()) {
                s = c * x - s
                c = c * (size - i) / (i + 1)
            }
            return s
        }
    }
}
