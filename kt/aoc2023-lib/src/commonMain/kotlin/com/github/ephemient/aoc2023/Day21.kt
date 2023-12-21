package com.github.ephemient.aoc2023

class Day21(input: String) {
    private val maze = input.lines().filter { it.isNotEmpty() }.apply {
        require(size % 2 == 1)
        require(all { it.length == size })
        require(first().all { it != '#' })
        require(all { it.first() != '#' && it.last() != '#' })
        require(last().all { it != '#' })
    }
    private val start = maze.withIndex().firstNotNullOf { (y, line) ->
        val x = line.indexOf('S')
        if (x >= 0) IntPair(y, x) else null
    }

    fun part1(n: Int = 64): Int {
        var acc = 0
        val steps = IntArray(maze.size * maze.size) { -1 }.apply { this[start] = 0 }
        val queue = ArrayDeque<IntPair>().apply { add(start) }
        @Suppress("LoopWithTooManyJumpStatements")
        while (true) {
            val point = queue.removeFirstOrNull() ?: break
            val step = steps[point]
            if (step > n) break
            if (step xor n and 1 == 0) acc++
            for (next in point.neighbors()) {
                if (steps[next] < 0) {
                    steps[next] = step + 1
                    queue.add(next)
                }
            }
        }
        return acc
    }

    private fun IntPair.neighbors(): List<IntPair> = buildList(4) {
        if (first > 0 && maze[first - 1][second] != '#') add(IntPair(first - 1, second))
        if (second > 0 && maze[first][second - 1] != '#') add(IntPair(first, second - 1))
        if (first < maze.lastIndex && maze[first + 1][second] != '#') add(IntPair(first + 1, second))
        if (second < maze.lastIndex && maze[first][second + 1] != '#') add(IntPair(first, second + 1))
    }

    private operator fun IntArray.get(pair: IntPair): Int = this[pair.first * maze.size + pair.second]
    private operator fun IntArray.set(pair: IntPair, value: Int) {
        this[pair.first * maze.size + pair.second] = value
    }
}
