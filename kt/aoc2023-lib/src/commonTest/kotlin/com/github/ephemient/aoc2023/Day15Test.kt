package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day15Test {
    @Test
    fun part1() {
        assertEquals(1320, Day15(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(145, Day15(example).part2())
    }

    companion object {
        private val example =
            """
            |rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
            |""".trimMargin()
    }
}
