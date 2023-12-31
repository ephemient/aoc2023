package com.github.ephemient.aoc2023

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

class Day16Test {
    @Test
    fun part1() {
        assertEquals(46, Day16(example).part1())
    }

    @Test
    fun part2() = runTest {
        assertEquals(51, Day16(example).part2())
    }

    companion object {
        private val example =
            """
            |.|...\....
            ||.-.\.....
            |.....|-...
            |........|.
            |..........
            |.........\
            |..../.\\..
            |.-.-/..|..
            |.|....-|.\
            |..//.|....
            |""".trimMargin()
    }
}
