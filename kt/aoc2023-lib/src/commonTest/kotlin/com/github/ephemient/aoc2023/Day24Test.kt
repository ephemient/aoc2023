package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day24Test {
    @Test
    fun part1() {
        assertEquals(2, Day24(example).part1(lo = 7.0, hi = 27.0))
    }

    @Test
    fun part2() {
        assertEquals(47, Day24(example).part2())
    }

    companion object {
        private val example =
            """
            |19, 13, 30 @ -2,  1, -2
            |18, 19, 22 @ -1, -1, -2
            |20, 25, 34 @ -2, -2, -4
            |12, 31, 28 @ -1, -2, -1
            |20, 19, 15 @  1, -5, -3
            |""".trimMargin()
    }
}
