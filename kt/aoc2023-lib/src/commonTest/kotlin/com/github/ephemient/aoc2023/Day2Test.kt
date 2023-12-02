package com.github.ephemient.aoc2023

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

class Day2Test {
    @Test
    fun part1() = runTest {
        assertEquals(8, Day2(example).part1())
    }

    @Test
    fun part2() = runTest {
        assertEquals(2286, Day2(example).part2())
    }

    companion object {
        private val example =
            """
            |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
            |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
            |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
            |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
            |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
            |""".trimMargin()
    }
}
