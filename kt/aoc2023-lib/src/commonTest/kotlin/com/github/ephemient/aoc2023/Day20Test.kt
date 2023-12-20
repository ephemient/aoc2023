package com.github.ephemient.aoc2023

import kotlin.test.Ignore
import kotlin.test.Test
import kotlin.test.assertEquals

class Day20Test {
    @Test
    fun part1() {
        assertEquals(32000000, Day20(example1).part1())
        assertEquals(11687500, Day20(example2).part1())
    }

    @Test
    @Ignore
    fun part2() {
        assertEquals(0, Day20("").part2())
    }

    companion object {
        private val example1 =
            """
            |broadcaster -> a, b, c
            |%a -> b
            |%b -> c
            |%c -> inv
            |&inv -> a
            |""".trimMargin()
        private val example2 =
            """
            |broadcaster -> a
            |%a -> inv, con
            |&inv -> b
            |%b -> con
            |&con -> output
            |""".trimMargin()
    }
}
