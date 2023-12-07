package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day7Test {
    @Test
    fun part1() {
        assertEquals(6440, Day7(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(5905, Day7(example).part2())
    }

    companion object {
        private val example =
            """
            |32T3K 765
            |T55J5 684
            |KK677 28
            |KTJJT 220
            |QQQJA 483
            |""".trimMargin()
    }
}
