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

    @Test
    fun part2() {
        assertEquals(16, Day21(example).part2(6))
        assertEquals(50, Day21(example).part2(10))
        assertEquals(1594, Day21(example).part2(50))
        assertEquals(6536, Day21(example).part2(100))
        assertEquals(167004, Day21(example).part2(500))
        assertEquals(668697, Day21(example).part2(1000))
        assertEquals(16733044, Day21(example).part2(5000))
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
