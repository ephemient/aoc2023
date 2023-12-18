package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day18Test {
    @Test
    fun part1() {
        assertEquals(62, Day18(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(952408144115, Day18(example).part2())
    }

    companion object {
        private val example =
            """
            |R 6 (#70c710)
            |D 5 (#0dc571)
            |L 2 (#5713f0)
            |D 2 (#d2c081)
            |R 2 (#59c680)
            |D 2 (#411b91)
            |L 5 (#8ceee2)
            |U 2 (#caa173)
            |L 1 (#1b58a2)
            |U 2 (#caa171)
            |R 2 (#7807d2)
            |U 3 (#a77fa3)
            |L 2 (#015232)
            |U 2 (#7a21e3)
            |""".trimMargin()
    }
}
