package com.github.ephemient.aoc2023

class Day1(input: String) {
    private val lines = input.lines()

    fun part1() = solve(digits)

    fun part2() = solve(extendedDigits)

    private fun solve(values: Collection<IndexedValue<String>>): Int = lines.sumOf { line ->
        val subset = values.filter { it.value in line }
        if (subset.isNotEmpty()) {
            10 * subset.minBy { line.indexOf(it.value) }.index + subset.maxBy { line.lastIndexOf(it.value) }.index
        } else {
            0
        }
    }

    companion object {
        private val digits = List(10) { IndexedValue(it, it.toString()) }
        private val extendedDigits = digits + listOf(
            IndexedValue(1, "one"),
            IndexedValue(2, "two"),
            IndexedValue(3, "three"),
            IndexedValue(4, "four"),
            IndexedValue(5, "five"),
            IndexedValue(6, "six"),
            IndexedValue(7, "seven"),
            IndexedValue(8, "eight"),
            IndexedValue(9, "nine"),
        )
    }
}
