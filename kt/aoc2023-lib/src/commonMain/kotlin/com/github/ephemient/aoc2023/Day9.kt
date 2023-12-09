package com.github.ephemient.aoc2023

class Day9(input: String) {
    private val nums = input.lines().mapNotNull { line ->
        line.split(' ').mapNotNull { it.toIntOrNull() }.ifEmpty { null }
    }

    fun part1(): Int = nums.sumOf { it.extrapolate() }

    fun part2(): Int = nums.sumOf { it.asReversed().extrapolate() }

    companion object {
        private fun List<Int>.extrapolate(): Int {
            var c = 1
            var s = 0
            for ((i, x) in withIndex()) {
                s = c * x - s
                c = c * (size - i) / (i + 1)
            }
            return s
        }
    }
}
