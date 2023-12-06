package com.github.ephemient.aoc2023

import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.sqrt

class Day6(private val input: String) {
    private val races = input.lines().let { (line1, line2) ->
        NUMBER.findAll(line1).map { it.value.toLong() } zip
            NUMBER.findAll(line2).map { it.value.toLong() }
    }

    fun part1(): Long = races.fold(1) { acc, (time, distance) ->
        acc * winCount(time, distance)
    }

    fun part2(): Long {
        val (time, distance) = NUMBER.findAll(input.replace(" ", "")).map { it.value.toLong() }.toList()
        return winCount(time, distance)
    }

    companion object {
        private val NUMBER = """\d+""".toRegex()

        private fun winCount(time: Long, distance: Long): Long {
            // x * (time - x) > distance
            // x^2 - time*x < -distance
            // x^2 - time*x + (time/2)^2 < (time/2)^2 - distance
            // abs(x - time/2) < sqrt((time/2)^2 - distance)
            val b = time / 2.0
            val d = sqrt(b * b - distance)
            println("$time/$distance = ${b - d}..${b + d}")
            return (floor(b + d) - ceil(b - d) + 1).toLong()
        }
    }
}
