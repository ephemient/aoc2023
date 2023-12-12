package com.github.ephemient.aoc2023

import kotlin.test.Test
import kotlin.test.assertEquals

class Day12Test {
    @Test
    fun part1() {
        assertEquals(21, Day12(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(525152, Day12(example).part2())
    }

    companion object {
        private val example =
            """
            |???.### 1,1,3
            |.??..??...?##. 1,1,3
            |?#?#?#?#?#?#?#? 1,3,1,6
            |????.#...#... 4,1,1
            |????.######..#####. 1,6,5
            |?###???????? 3,2,1
            |""".trimMargin()
    }
}
