package com.github.ephemient.aoc2023

class Day16(input: String) {
    private val input = input.lines().filter { it.isNotEmpty() }

    private fun fill(y: Int, x: Int, d: Direction, todo: MutableSet<Pair<IntPair, Direction>>): Int {
        val stack = mutableListOf(y to x to d)
        val visited = stack.toMutableSet()
        while (true) {
            val (p1, d1) = stack.removeLastOrNull() ?: break
            for (d2 in lut[d1 to input[p1.first][p1.second]] ?: listOf(d1)) {
                val p2 = p1.move(d2)
                if (p2.first in input.indices && p2.second in input[p2.first].indices) {
                    val next = p2 to d2
                    visited.add(next) && stack.add(next)
                } else {
                    todo.remove(p1 to -d1)
                }
            }
        }
        return visited.mapTo(mutableSetOf()) { it.first }.size
    }

    fun part1(): Int = fill(0, 0, Direction.R, mutableSetOf())

    fun part2(): Int {
        var max = 0
        val todo = mutableSetOf<Pair<IntPair, Direction>>()
        for (x in 0..input.first().lastIndex) todo.add(0 to x to Direction.D)
        for ((y, line) in input.withIndex()) {
            todo.add(y to 0 to Direction.R)
            todo.add(y to line.lastIndex to Direction.L)
        }
        for (x in 0..input.last().lastIndex) todo.add(input.lastIndex to x to Direction.U)
        while (true) {
            val iterator = todo.iterator()
            if (!iterator.hasNext()) break
            val (pos, d) = iterator.next()
            iterator.remove()
            max = maxOf(max, fill(pos.first, pos.second, d, todo))
        }
        return max
    }

    private enum class Direction {
        U, L, D, R,
        ;

        operator fun unaryMinus(): Direction = when (this) {
            U -> D
            L -> R
            D -> U
            R -> L
        }
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
