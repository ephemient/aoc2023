package com.github.ephemient.aoc2023

import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.sqrt

class Day6(private val input: String) {
    private val races = input.lines().let { (line1, line2) ->
        NUMBER.findAll(line1).map { it.value.toInt() } zip
            NUMBER.findAll(line2).map { it.value.toInt() }
    }

    fun part1(): Int = races.fold(1) { acc, (time, distance) ->
        acc * winCount(time, distance)
    }

    fun part2(): Int {
        val (time, distance) = NUMBER.findAll(input.replace(" ", "")).map { it.value.toInt() }.toList()
        println(time)
        println(distance)
        return winCount(time, distance)
    }

    companion object {
        private val NUMBER = """\d+""".toRegex()

        private fun winCount(time: Int, distance: Int): Int {
            // x * (time - x) > distance
            // x^2 - time*x < -distance
            // x^2 - time*x + (time/2)^2 < (time/2)^2 - distance
            // abs(x - time/2) < sqrt((time/2)^2 - distance)
            val b = time / 2.0
            val d = sqrt(b * b - distance)
            return (ceil(b - d) - floor(b + d) + 1).toInt()
        }
    }
}
