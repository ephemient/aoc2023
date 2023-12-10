package com.github.ephemient.aoc2023

import kotlin.math.absoluteValue

class Day10(input: String) {
    private val maze = input.lines()
    private lateinit var path: List<IntPair>

    @Suppress("NestedBlockDepth")
    fun part1(): Int {
        for ((y, line) in maze.withIndex()) {
            for ((x, char) in line.withIndex()) {
                if (char != 'S') continue
                val startPos = y to x
                for (startDir in Direction.entries) {
                    var pos = startPos
                    val path = buildList {
                        var dir = startDir
                        while (true) {
                            add(pos)
                            pos = pos.move(dir)
                            dir = lut[dir to maze.getOrNull(pos.first)?.getOrNull(pos.second)] ?: break
                        }
                    }.toList()
                    if (pos == startPos) {
                        this.path = path
                        return path.size / 2
                    }
                }
            }
        }
        @Suppress("UseCheckOrError")
        throw IllegalStateException("No loop found")
    }

    fun part2(): Int {
        if (!::path.isInitialized) part1()
        return 1 + (
            path.asSequence().plus(path[0]).zipWithNext { (y0, x0), (y1, x1) ->
                x0 * y1 - x1 * y0
            }.sum().absoluteValue - path.size
            ) / 2
    }

    private enum class Direction {
        U, L, D, R,
    }

    companion object {
        private val lut = mapOf(
            Direction.U to '|' to Direction.U,
            Direction.U to '7' to Direction.L,
            Direction.U to 'F' to Direction.R,
            Direction.L to '-' to Direction.L,
            Direction.L to 'F' to Direction.D,
            Direction.L to 'L' to Direction.U,
            Direction.D to '|' to Direction.D,
            Direction.D to 'L' to Direction.R,
            Direction.D to 'J' to Direction.L,
            Direction.R to '-' to Direction.R,
            Direction.R to 'J' to Direction.U,
            Direction.R to '7' to Direction.D,
        )

        private fun IntPair.move(dir: Direction) = when (dir) {
            Direction.U -> first - 1 to second
            Direction.L -> first to second - 1
            Direction.D -> first + 1 to second
            Direction.R -> first to second + 1
        }
    }
}
