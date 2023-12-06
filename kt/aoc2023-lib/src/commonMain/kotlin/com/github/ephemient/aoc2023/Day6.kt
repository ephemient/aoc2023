package com.github.ephemient.aoc2023

import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.roundToInt
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
            // x^2 - time*x + time^2/4 < time^2/4 - distance
            // (x - time/2)^2 < sqrt(time^2/4 - distance)
            // x < time/2 + sqrt(time^2/4 - distance)
            val lo = time / 2.0 - sqrt(time * time / 4.0 - distance)
            val hi = time / 2.0 + sqrt(time * time / 4.0 - distance)
            println("$time/$distance $lo..$hi")
            check(0 <= lo && lo <= hi && hi <= time)
            return (floor(Double.fromBits(hi.toRawBits() - 1)) - ceil(Double.fromBits(lo.toRawBits() + 1)) + 1).roundToInt()
        }
    }
}
