package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day11Test {
    @Test
    fun part1() {
        assertEquals(374, Day11(example).solve(2))
    }

    @Test
    fun part2() {
        assertEquals(1030, Day11(example).solve(10))
        assertEquals(8410, Day11(example).solve(100))
    }

    companion object {
        private val example =
            """
            |...#......
            |.......#..
            |#.........
            |..........
            |......#...
            |.#........
            |.........#
            |..........
            |.......#..
            |#...#.....
            |""".trimMargin()
    }
}
