package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day14Test {
    @Test
    fun part1() {
        assertEquals(136, Day14(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(64, Day14(example).part2())
    }

    companion object {
        private val example =
            """
            |O....#....
            |O.OO#....#
            |.....##...
            |OO.#O....O
            |.O.....O#.
            |O.#..O.#.#
            |..O..#O..O
            |.......O..
            |#....###..
            |#OO..#....
            |""".trimMargin()
    }
}
