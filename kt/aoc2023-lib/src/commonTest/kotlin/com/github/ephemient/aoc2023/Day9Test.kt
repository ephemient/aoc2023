package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day9Test {
    @Test
    fun part1() {
        assertEquals(114, Day9(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(2, Day9(example).part2())
    }

    companion object {
        private val example =
            """
            |0 3 6 9 12 15
            |1 3 6 10 15 21
            |10 13 16 21 30 45
            |""".trimMargin()
    }
}
