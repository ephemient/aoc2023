package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day10Test {
    @Test
    fun part1() {
        assertEquals(4, Day10(example1, debug = true).part1())
        assertEquals(8, Day10(example2, debug = true).part1())
    }

    @Test
    fun part2() {
        assertEquals(4, Day10(example3, debug = true).part2())
        assertEquals(4, Day10(example4, debug = true).part2())
        assertEquals(8, Day10(example5, debug = true).part2())
        assertEquals(10, Day10(example6, debug = true).part2())
    }

    companion object {
        private val example1 =
            """
            |-L|F7
            |7S-7|
            |L|7||
            |-L-J|
            |L|-JF
            |""".trimMargin()
        private val example2 =
            """
            |7-F7-
            |.FJ|7
            |SJLL7
            ||F--J
            |LJ.LJ
            |""".trimMargin()
        private val example3 =
            """
            |...........
            |.S-------7.
            |.|F-----7|.
            |.||.....||.
            |.||.....||.
            |.|L-7.F-J|.
            |.|..|.|..|.
            |.L--J.L--J.
            |...........
            |""".trimMargin()
        private val example4 =
            """
            |..........
            |.S------7.
            |.|F----7|.
            |.||....||.
            |.||....||.
            |.|L-7F-J|.
            |.|..||..|.
            |.L--JL--J.
            |..........
            |""".trimMargin()
        private val example5 =
            """
            |.F----7F7F7F7F-7....
            |.|F--7||||||||FJ....
            |.||.FJ||||||||L7....
            |FJL7L7LJLJ||LJ.L-7..
            |L--J.L7...LJS7F-7L7.
            |....F-J..F7FJ|L7L7L7
            |....L7.F7||L7|.L7L7|
            |.....|FJLJ|FJ|F7|.LJ
            |....FJL-7.||.||||...
            |....L---J.LJ.LJLJ...
            |""".trimMargin()
        private val example6 =
            """
            |FF7FSF7F7F7F7F7F---7
            |L|LJ||||||||||||F--J
            |FL-7LJLJ||||||LJL-77
            |F--JF--7||LJLJ7F7FJ-
            |L---JF-JLJ.||-FJLJJ7
            ||F|F-JF---7F7-L7L|7|
            ||FFJF7L7F-JF7|JL---7
            |7-L-JL7||F7|L7F-7F7|
            |L.L7LFJ|||||FJL7||LJ
            |L7JLJL-JLJLJL--JLJ.L
            |""".trimMargin()
    }
}
