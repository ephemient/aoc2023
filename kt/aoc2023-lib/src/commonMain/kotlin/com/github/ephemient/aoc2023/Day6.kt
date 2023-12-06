package com.github.ephemient.aoc2023

class Day6(input: String) {
    private val races = input.lines().let { (line1, line2) ->
        NUMBER.findAll(line1).map { it.value.toInt() } zip
            NUMBER.findAll(line2).map { it.value.toInt() }
    }

    fun part1(): Int = 0

    fun part2(): Int = 0

    companion object {
        private val NUMBER = """\d+""".toRegex()

        private fun winCount(time: Int, distance: Int): Int {
            // wait
        }
    }
}
