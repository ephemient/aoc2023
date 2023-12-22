package com.github.ephemient.aoc2023

class Day21(input: String) {
    private val grid = input.lines().filter { it.isNotEmpty() }
    private val size = grid.size
    init {
        require(size % 2 != 0)
        require(grid.all { it.length == size })
        require(grid.first().all { it != '#' })
        require(grid.all { it.first() != '#' && it.last() != '#' })
        require(grid.last().all { it != '#' })
    }
    private val start = grid.withIndex().firstNotNullOf { (y, line) ->
        val x = line.indexOf('S')
        if (x >= 0) IntPair(y, x) else null
    }

    private fun makeBlock(initial: Iterable<IndexedValue<IntPair>>): IntArray {
        val queue = PriorityQueue<IndexedValue<IntPair>>(compareBy { it.index })
        val block = IntArray(size * size) { -1 }
        for (value in initial) queue.add(value)
        while (!queue.isEmpty()) {
            val (step, point) = queue.remove()
            val existing = block[point]
            if (existing in 0..step) continue
            check(existing < 0)
            block[point] = step
            for (next in point.neighbors()) {
                val candidate = block[next]
                if (candidate < 0) {
                    queue.add(IndexedValue(step + 1, next))
                } else {
                    check(candidate in step - 1..step + 1) { "$point=$step $next=$candidate" }
                }
            }
        }
        return block
    }

    fun part1(n: Int = 64): Int = makeBlock(listOf(IndexedValue(0, start))).count { it in 0..n && it xor n and 1 == 0 }

    @Suppress("CyclomaticComplexMethod")
    fun part2(n: Int = 26501365): Long {
        val origin = makeBlock(listOf(IndexedValue(0, start)))
        var acc = origin.count { it in 0..n && it xor n and 1 == 0 }.toLong()

        for (quadrant in 0..<4) {
            val signY = quadrant and 1 == 0
            val signX = quadrant and 2 == 0
            val block = makeBlock(
                listOf(
                    IndexedValue(
                        origin[if (signY) 0 else size - 1, if (signX) 0 else size - 1] + 2,
                        IntPair(if (signY) size - 1 else 0, if (signX) size - 1 else 0)
                    )
                )
            )
            acc += block.sumOf { step ->
                if (step !in 0..n) return@sumOf 0
                val remaining = n - step
                if (remaining % 2 == 0) {
                    (remaining / size / 2 + 1).let { it.toLong() * it }
                } else {
                    ((remaining / size + 1) / 2).let { it.toLong() * (it + 1) }
                }
            }
        }

        for (axis in 0..<4) {
            val sign = axis and 1 == 0
            val src = if (sign) 0 else size - 1
            val dst = if (sign) size - 1 else 0
            val orientation = axis and 2 == 0
            var block = origin
            do {
                val lastBlock = block
                block = makeBlock(
                    (0..<size).map {
                        IndexedValue(
                            lastBlock[if (orientation) it else src, if (orientation) src else it] + 1,
                            IntPair(if (orientation) it else dst, if (orientation) dst else it),
                        )
                    }
                )
                acc += block.count { it in 0..n && it xor n and 1 == 0 }
            } while (
                block.any { it in 0..n } &&
                block.withIndex().any { (i, step) -> step >= 0 && step - lastBlock[i] != size }
            )

            acc += block.sumOf { step ->
                if (step < 0) return@sumOf 0
                val remaining = n - step + size
                ((remaining + 1) / size - remaining.and(1)).coerceAtLeast(0) / 2
            }
        }

        return acc
    }

    private fun IntPair.neighbors(): List<IntPair> = buildList(4) {
        if (first > 0 && grid[first - 1][second] != '#') add(IntPair(first - 1, second))
        if (second > 0 && grid[first][second - 1] != '#') add(IntPair(first, second - 1))
        if (first < this@Day21.size - 1 && grid[first + 1][second] != '#') add(IntPair(first + 1, second))
        if (second < this@Day21.size - 1 && grid[first][second + 1] != '#') add(IntPair(first, second + 1))
    }

    private operator fun IntArray.get(y: Int, x: Int): Int = this[y * this@Day21.size + x]
    private operator fun IntArray.get(pair: IntPair): Int = this[pair.first, pair.second]
    private operator fun IntArray.set(y: Int, x: Int, value: Int) {
        this[y * this@Day21.size + x] = value
    }
    private operator fun IntArray.set(pair: IntPair, value: Int) {
        this[pair.first, pair.second] = value
    }
}
