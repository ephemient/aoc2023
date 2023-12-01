package com.github.ephemient.aoc2023

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

class Day1Test {
    @Test
    fun part1() = runTest {
        assertEquals(142, Day1(example1).part1())
    }

    @Test
    fun part2() = runTest {
        assertEquals(281, Day1(example2).part2())
    }

    companion object {
        private val example1 =
            """
            |1abc2
            |pqr3stu8vwx
            |a1b2c3d4e5f
            |treb7uchet
            |""".trimMargin()

        private val example2 =
            """
            |two1nine
            |eightwothree
            |abcone2threexyz
            |xtwone3four
            |4nineeightseven2
            |zoneight234
            |7pqrstsixteen
            |""".trimMargin()
    }
}
