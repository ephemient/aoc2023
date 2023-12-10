package com.github.ephemient.aoc2023

class Day10(input: String) {
    private val maze = input.lines()
    private val height = maze.size
    private val width = maze.maxOf { it.length }
    private lateinit var start: IntPair
    private lateinit var startDirs: List<Direction>
    private lateinit var path: List<IntPair>

    @Suppress("NestedBlockDepth")
    fun part1(): Int {
        for ((y, line) in maze.withIndex()) {
            for ((x, char) in line.withIndex()) {
                if (char != 'S') continue
                val start = y to x
                dir@for (startDir in Direction.entries) {
                    var pos = start.move(startDir)
                    var lastDir = -startDir
                    val path = mutableListOf(start)
                    while (pos != start) {
                        val dir = symbols[maze.getOrNull(pos.first)?.getOrNull(pos.second)]
                            ?.takeIf { lastDir in it }
                            ?.singleOrNull { it != lastDir }
                            ?: continue@dir
                        path += pos
                        pos = pos.move(dir)
                        lastDir = -dir
                    }
                    this.start = start
                    this.startDirs = listOf(startDir, lastDir)
                    this.path = path
                    return path.size / 2
                }
            }
        }
        @Suppress("UseCheckOrError")
        throw IllegalStateException("No loop found")
    }

    @Suppress("NestedBlockDepth")
    fun part2(): Int {
        if (!::start.isInitialized) part1()
        val path = path.sortedWith(compareBy(IntPair::first, IntPair::second))
        var count = 0
        var up = false
        var down = false
        var pathIndex = 0
        for (y in 0 until height) {
            for (x in 0 until width) {
                if (pathIndex < path.size && path[pathIndex].let { it.first == y && it.second == x }) {
                    pathIndex++
                    val dirs = if (start.first == y && start.second == x) startDirs else symbols[maze[y][x]]!!
                    up = up != dirs.contains(Direction.U)
                    down = down != dirs.contains(Direction.D)
                } else {
                    if (up && down) count++
                    check(up == down)
                }
            }
        }
        check(!up && !down && pathIndex == path.size)
        return count
    }

    private enum class Direction {
        U, L, D, R,
    }

    companion object {
        private val symbols = mapOf(
            '|' to listOf(Direction.U, Direction.D),
            '-' to listOf(Direction.L, Direction.R),
            'L' to listOf(Direction.U, Direction.R),
            'J' to listOf(Direction.U, Direction.L),
            '7' to listOf(Direction.L, Direction.D),
            'F' to listOf(Direction.D, Direction.R),
        )

        private operator fun Direction.unaryMinus(): Direction = when (this) {
            Direction.U -> Direction.D
            Direction.L -> Direction.R
            Direction.D -> Direction.U
            Direction.R -> Direction.L
        }

        private fun IntPair.move(dir: Direction) = when (dir) {
            Direction.U -> first - 1 to second
            Direction.L -> first to second - 1
            Direction.D -> first + 1 to second
            Direction.R -> first to second + 1
        }
    }
}
