package com.github.ephemient.aoc2023

import kotlinx.coroutines.flow.channelFlow
import kotlinx.coroutines.flow.fold
import kotlinx.coroutines.launch

class Day23(private val input: String) {
    private val src: IntPair
    private val dst: IntPair
    init {
        val lines = input.lines()
        src = lines.withIndex().firstNotNullOf { (y, line) ->
            val x = line.indexOf('.')
            if (x >= 0) IntPair(y, x) else null
        }
        dst = lines.asReversed().withIndex().firstNotNullOf { (y, line) ->
            val x = line.lastIndexOf('.')
            if (x >= 0) IntPair(lines.lastIndex - y, x) else null
        }
    }

    suspend fun part1(): Int = solve(input)

    suspend fun part2(): Int = solve(input.replace(slopes, "."))

    private suspend fun solve(input: String) = channelFlow {
        val gr = mkGraph(input)
        suspend fun go(src: IntPair, used: Set<IntPair>) {
            if (src == dst) return send(used.size)
            val used2 = used + src
            for (next in gr[src] ?: return) if (next !in used) launch { go(next, used2) }
        }
        go(src, emptySet())
    }.fold(0, ::maxOf)

    companion object {
        private val slopes = """[<>^v]""".toRegex()

        private fun mkGraph(input: String): Map<IntPair, List<IntPair>> = buildMap {
            val lines = input.lines()
            for ((y, line) in lines.withIndex()) {
                for ((x, char) in line.withIndex()) {
                    this[IntPair(y, x)] = when (char) {
                        '.' -> buildList {
                            if (lines.getOrNull(y - 1)?.getOrNull(x)?.let(".<^>"::contains) == true) {
                                add(IntPair(y - 1, x))
                            }
                            if (lines.getOrNull(y + 1)?.getOrNull(x)?.let(".<v>"::contains) == true) {
                                add(IntPair(y + 1, x))
                            }
                            if (line.getOrNull(x - 1)?.let(".^<v"::contains) == true) add(IntPair(y, x - 1))
                            if (line.getOrNull(x + 1)?.let(".^>v"::contains) == true) add(IntPair(y, x + 1))
                        }

                        '^' -> listOf(IntPair(y - 1, x))
                        'v' -> listOf(IntPair(y + 1, x))
                        '<' -> listOf(IntPair(y, x - 1))
                        '>' -> listOf(IntPair(y, x + 1))
                        else -> continue
                    }
                }
            }
        }
    }
}
