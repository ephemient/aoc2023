package com.github.ephemient.aoc2023

import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.roundToInt
import kotlin.math.sqrt

class Day6(input: String) {
    private val races = input.lines().let { (line1, line2) ->
        NUMBER.findAll(line1).map { it.value.toInt() } zip
            NUMBER.findAll(line2).map { it.value.toInt() }
    }

    fun part1(): Int = races.fold(1) { acc, (time, distance) ->
        println("$time/$distance = ${winCount(time, distance)}")
        acc * winCount(time, distance)
    }

    fun part2(): Int = 0

    companion object {
        private val NUMBER = """\d+""".toRegex()

        private fun winCount(time: Int, distance: Int): Int {
            return (0..time).count { it * (time - it) > distance }
        }
    }
}
