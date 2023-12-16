package com.github.ephemient.aoc2023

import kotlinx.coroutines.flow.channelFlow
import kotlinx.coroutines.flow.reduce
import kotlinx.coroutines.launch

class Day16(input: String) {
    private val input = input.lines().filter { it.isNotEmpty() }

    private fun fill(y: Int, x: Int, d: Direction): Int {
        val stack = mutableListOf(y to x to d)
        val visited = stack.toMutableSet()
        while (true) {
            val (p1, d1) = stack.removeLastOrNull() ?: break
            for (d2 in lut[d1 to input[p1.first][p1.second]] ?: listOf(d1)) {
                val p2 = p1.move(d2)
                if (p2.first in input.indices && p2.second in input[p2.first].indices) {
                    val next = p2 to d2
                    visited.add(next) && stack.add(next)
                }
            }
        }
        return visited.mapTo(mutableSetOf()) { it.first }.size
    }

    fun part1(): Int = fill(0, 0, Direction.R)

    suspend fun part2(): Int = channelFlow {
        for (x in 0..input.first().lastIndex) {
            launch { send(fill(0, x, Direction.D)) }
        }
        for ((y, line) in input.withIndex()) {
            launch { send(fill(y, 0, Direction.R)) }
            launch { send(fill(y, line.lastIndex, Direction.L)) }
        }
        for (x in 0..input.last().lastIndex) {
            launch { send(fill(input.lastIndex, x, Direction.U)) }
        }
    }.reduce(::maxOf)

    private enum class Direction {
        U, L, D, R,
    }

    companion object {
        private val lut = mapOf(
            Direction.U to '/' to listOf(Direction.R),
            Direction.U to '\\' to listOf(Direction.L),
            Direction.U to '-' to listOf(Direction.L, Direction.R),
            Direction.L to '/' to listOf(Direction.D),
            Direction.L to '\\' to listOf(Direction.U),
            Direction.L to '|' to listOf(Direction.U, Direction.D),
            Direction.D to '/' to listOf(Direction.L),
            Direction.D to '\\' to listOf(Direction.R),
            Direction.D to '-' to listOf(Direction.L, Direction.R),
            Direction.R to '/' to listOf(Direction.U),
            Direction.R to '\\' to listOf(Direction.D),
            Direction.R to '|' to listOf(Direction.U, Direction.D),
        )

        private fun IntPair.move(dir: Direction) = when (dir) {
            Direction.U -> first - 1 to second
            Direction.L -> first to second - 1
            Direction.D -> first + 1 to second
            Direction.R -> first to second + 1
        }
    }
}
