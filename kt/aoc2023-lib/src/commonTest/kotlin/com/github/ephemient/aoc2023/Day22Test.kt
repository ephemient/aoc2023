package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day22Test {
    @Test
    fun part1() {
        assertEquals(5, Day22(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(7, Day22(example).part2())
    }

    companion object {
        private val example =
            """
            |1,0,1~1,2,1
            |0,0,2~2,0,2
            |0,2,3~2,2,3
            |0,0,4~0,2,4
            |2,0,5~2,2,5
            |0,1,6~2,1,6
            |1,1,8~1,1,9
            |""".trimMargin()
    }
}
