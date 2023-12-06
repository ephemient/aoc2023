package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun part1() {
        assertEquals(288, Day6(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(71503, Day6(example).part2())
    }

    companion object {
        private val example =
            """
            |Time:      7  15   30
            |Distance:  9  40  200
            |""".trimMargin()
    }
}
