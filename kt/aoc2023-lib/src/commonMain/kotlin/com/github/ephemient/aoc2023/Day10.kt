package com.github.ephemient.aoc2023

class Day10(input: String, private val debug: Boolean = false) {
    private val maze = input.lines()
    private val height = maze.size
    private val width = maze.maxOf { it.length }
    private lateinit var loop: Set<IntPair>

    @Suppress("CyclomaticComplexMethod")
    fun part1(): Int {
        val start = maze.mapIndexedNotNull { y, line ->
            val x = line.indexOf('S')
            require(line.indexOf('S', startIndex = x + 1) == -1)
            if (x >= 0) y to x else null
        }.single()
        val queue = PriorityQueue<IndexedValue<IntPair>>(compareBy { it.index })
        queue.add(IndexedValue(0, start))
        var lastD = -1
        loop = buildSet {
            @Suppress("LoopWithTooManyJumpStatements")
            while (!queue.isEmpty()) {
                val (d, position) = queue.remove()
                if (!add(position)) continue
                val (y, x) = position
                when (maze[position]) {
                    '|' -> {
                        if (y > 0) queue.add(IndexedValue(d + 1, y - 1 to x))
                        if (y + 1 < height) queue.add(IndexedValue(d + 1, y + 1 to x))
                    }
                    '-' -> {
                        if (x > 0) queue.add(IndexedValue(d + 1, y to x - 1))
                        if (x + 1 < width) queue.add(IndexedValue(d + 1, y to x + 1))
                    }
                    'L' -> {
                        if (y > 0) queue.add(IndexedValue(d + 1, y - 1 to x))
                        if (x + 1 < width) queue.add(IndexedValue(d + 1, y to x + 1))
                    }
                    'J' -> {
                        if (y > 0) queue.add(IndexedValue(d + 1, y - 1 to x))
                        if (x > 0) queue.add(IndexedValue(d + 1, y to x - 1))
                    }
                    '7' -> {
                        if (x > 0) queue.add(IndexedValue(d + 1, y to x - 1))
                        if (y + 1 < height) queue.add(IndexedValue(d + 1, y + 1 to x))
                    }
                    'F' -> {
                        if (x + 1 < width) queue.add(IndexedValue(d + 1, y to x + 1))
                        if (y + 1 < height) queue.add(IndexedValue(d + 1, y + 1 to x))
                    }
                    'S' -> {
                        if (maze[y - 1, x] in "|7F") queue.add(IndexedValue(d + 1, y - 1 to x))
                        if (maze[y, x - 1] in "-LF") queue.add(IndexedValue(d + 1, y to x - 1))
                        if (maze[y + 1, x] in "|LJ") queue.add(IndexedValue(d + 1, y + 1 to x))
                        if (maze[y, x + 1] in "-J7") queue.add(IndexedValue(d + 1, y to x + 1))
                    }
                    else -> continue
                }
                lastD = d
            }
        }
        return lastD
    }

    @Suppress("CyclomaticComplexMethod")
    fun part2(): Int {
        if (!::loop.isInitialized) part1()
        val fill = buildSet<IntPair> {
            val f = DeepRecursiveFunction { position: IntPair ->
                if (!add(position)) return@DeepRecursiveFunction
                val (y, x) = position
                val ul = if (y - 1 to x - 1 in loop) maze[y - 1, x - 1] else '.'
                val ur = if (y - 1 to x in loop) maze[y - 1, x] else '.'
                val dl = if (y to x - 1 in loop) maze[y, x - 1] else '.'
                val dr = if (y to x in loop) maze[y, x] else '.'
                if (y > 0 && ul in "|J7." && ur in "|LF.") callRecursive(y - 1 to x)
                if (x > 0 && ul in "-LJ." && dl in "-7F.") callRecursive(y to x - 1)
                if (y < height && dl in "|J7." && dr in "|LF.") callRecursive(y + 1 to x)
                if (x < width && ur in "-LJ." && dr in "-7F.") callRecursive(y to x + 1)
            }
            for (x in 0..width) f(0 to x)
            for (y in 0..height) f(y to 0)
            for (x in 0..width) f(height to x)
            for (y in 0..height) f(y to width)
        }
        val tmp = fill.toMutableSet().apply { retainAll { (y, x) -> y to x + 1 in fill } }
        val outside = tmp.toMutableSet().apply { retainAll { (y, x) -> y + 1 to x in tmp } }

        if (debug) {
            check(loop.intersect(outside).isEmpty())
            for (y in 0 until height) {
                println(
                    (0 until width).joinToString("") { x ->
                        when (val position = y to x) {
                            in loop -> maze[position]
                            in outside -> 'O'
                            else -> 'I'
                        }.toString()
                    }
                )
            }
            println()
        }

        return width * height - loop.union(outside).size
    }

    companion object {
        @Suppress("ReturnCount")
        private operator fun List<String>.get(y: Int, x: Int): Char {
            if (y !in indices) return '.'
            val line = get(y)
            if (x !in line.indices) return '.'
            return line[x]
        }

        private operator fun List<String>.get(position: IntPair): Char =
            get(position.first, position.second)
    }
}
