package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day3Test {
    @Test
    fun part1() {
        assertEquals(4361, Day3(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(467835, Day3(example).part2())
    }

    companion object {
        private val example =
            """
            |467..114..
            |...*......
            |..35..633.
            |......#...
            |617*......
            |.....+.58.
            |..592.....
            |......755.
            |...$.*....
            |.664.598..
            |""".trimMargin()
    }
}
