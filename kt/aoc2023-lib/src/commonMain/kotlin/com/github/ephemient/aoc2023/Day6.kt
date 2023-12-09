package com.github.ephemient.aoc2023

import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.sqrt

class Day6(private val input: String) {
    fun part1(): Long = NUMBER.findAll(input.substringBefore('\n')).map { it.value.toLong() }
        .zip(NUMBER.findAll(input.substringAfter('\n')).map { it.value.toLong() }, ::winCount)
        .fold(1, Long::times)

    fun part2(): Long = winCount(
        input.substringBefore('\n').filter(Char::isDigit).toLong(),
        input.substringAfter('\n').filter(Char::isDigit).toLong(),
    )

    companion object {
        private val NUMBER = """\d+""".toRegex()

        private fun winCount(time: Long, distance: Long): Long {
            // x * (time - x) > distance
            // x^2 - time*x < -distance
            // x^2 - time*x + (time/2)^2 < (time/2)^2 - distance
            // sqrt((x - time/2)^2) < sqrt((time/2)^2 - distance)
            // abs(x - time/2) < sqrt((time/2)^2 - distance)
            val b = time / 2.0
            val d = sqrt(b * b - distance)
            return (ceil(b + d - 1) - floor(b - d + 1) + 1).toLong()
        }
    }
}
