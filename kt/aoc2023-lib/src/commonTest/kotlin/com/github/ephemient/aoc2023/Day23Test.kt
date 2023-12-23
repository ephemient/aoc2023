package com.github.ephemient.aoc2023

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

class Day23Test {
    @Test
    fun part1() = runTest {
        assertEquals(94, Day23(example).part1())
    }

    @Test
    fun part2() = runTest {
        assertEquals(154, Day23(example).part2())
    }

    companion object {
        private val example =
            """
            |#.#####################
            |#.......#########...###
            |#######.#########.#.###
            |###.....#.>.>.###.#.###
            |###v#####.#v#.###.#.###
            |###.>...#.#.#.....#...#
            |###v###.#.#.#########.#
            |###...#.#.#.......#...#
            |#####.#.#.#######.#.###
            |#.....#.#.#.......#...#
            |#.#####.#.#.#########v#
            |#.#...#...#...###...>.#
            |#.#.#v#######v###.###v#
            |#...#.>.#...>.>.#.###.#
            |#####v#.#.###v#.#.###.#
            |#.....#...#...#.#.#...#
            |#.#########.###.#.#.###
            |#...###...#...#...#.###
            |###.###.#.###v#####v###
            |#...#...#.#.>.>.#.>.###
            |#.###.###.#.###.#.#v###
            |#.....###...###...#...#
            |#####################.#
            |""".trimMargin()
    }
}
