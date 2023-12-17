package com.github.ephemient.aoc2023

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

class Day17Test {
    @Test
    fun part1() {
        assertEquals(102, Day17(example1).part1())
    }

    @Test
    fun part2() = runTest {
        assertEquals(94, Day17(example1).part2())
        assertEquals(71, Day17(example2).part2())
    }

    companion object {
        private val example1 =
            """
            |2413432311323
            |3215453535623
            |3255245654254
            |3446585845452
            |4546657867536
            |1438598798454
            |4457876987766
            |3637877979653
            |4654967986887
            |4564679986453
            |1224686865563
            |2546548887735
            |4322674655533
            |""".trimMargin()
        private val example2 =
            """
            |111111111111
            |999999999991
            |999999999991
            |999999999991
            |999999999991
            """.trimMargin()
    }
}
