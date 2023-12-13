package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day13Test {
    @Test
    fun part1() {
        assertEquals(405, Day13(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(400, Day13(example).part2())
    }

    companion object {
        private val example =
            """
            |#.##..##.
            |..#.##.#.
            |##......#
            |##......#
            |..#.##.#.
            |..##..##.
            |#.#.##.#.
            |
            |#...##..#
            |#....#..#
            |..##..###
            |#####.##.
            |#####.##.
            |..##..###
            |#....#..#
            |""".trimMargin()
    }
}
