package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day8Test {
    @Test
    fun part1() {
        assertEquals(2, Day8(example1).part1())
        assertEquals(6, Day8(example2).part1())
    }

    @Test
    fun part2() {
        assertEquals(6, Day8(example3).part2())
    }

    companion object {
        private val example1 =
            """
            |RL
            |
            |AAA = (BBB, CCC)
            |BBB = (DDD, EEE)
            |CCC = (ZZZ, GGG)
            |DDD = (DDD, DDD)
            |EEE = (EEE, EEE)
            |GGG = (GGG, GGG)
            |ZZZ = (ZZZ, ZZZ)
            |""".trimMargin()
        private val example2 =
            """
            |LLR
            |
            |AAA = (BBB, BBB)
            |BBB = (AAA, ZZZ)
            |ZZZ = (ZZZ, ZZZ)
            |""".trimMargin()
        private val example3 =
            """
            |LR
            |
            |11A = (11B, XXX)
            |11B = (XXX, 11Z)
            |11Z = (11B, XXX)
            |22A = (22B, XXX)
            |22B = (22C, 22C)
            |22C = (22Z, 22Z)
            |22Z = (22B, 22B)
            |XXX = (XXX, XXX)
            |""".trimMargin()
    }
}
