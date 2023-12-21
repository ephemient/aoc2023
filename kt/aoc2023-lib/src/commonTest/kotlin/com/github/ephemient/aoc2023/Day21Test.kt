package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day21Test {
    @Test
    fun part1() {
        assertEquals(2, Day21(example).part1(1))
        assertEquals(4, Day21(example).part1(2))
        assertEquals(6, Day21(example).part1(3))
        assertEquals(16, Day21(example).part1(6))
    }

    companion object {
        private val example =
            """
            |...........
            |.....###.#.
            |.###.##..#.
            |..#.#...#..
            |....#.#....
            |.##..S####.
            |.##..#...#.
            |.......##..
            |.##.#.####.
            |.##..##.##.
            |...........
            |""".trimMargin()
    }
}
