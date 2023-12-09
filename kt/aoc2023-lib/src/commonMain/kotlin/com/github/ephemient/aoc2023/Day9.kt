package com.github.ephemient.aoc2023

class Day9(input: String) {
    private val nums = input.lines().mapNotNull { line ->
        line.split(' ').mapNotNull { it.toIntOrNull() }.ifEmpty { null }
    }

    fun part1(): Int = nums.sumOf { it.extrapolate() }

    fun part2(): Int = nums.sumOf { it.asReversed().extrapolate() }

    companion object {
        private fun List<Int>.extrapolate(): Int = if (any { it != 0 }) {
            last() + zipWithNext { a, b -> b - a }.extrapolate()
        } else {
            0
        }
    }
}
