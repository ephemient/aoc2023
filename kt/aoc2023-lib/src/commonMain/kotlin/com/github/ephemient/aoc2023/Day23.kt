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

    suspend fun part1(): Int = solve(false)

    suspend fun part2(): Int = solve(true)

    private suspend fun solve(simplify: Boolean) = channelFlow {
        val gr = mkGraph(simplify)
        suspend fun go(src: IntPair, used: Set<IntPair>, distance: Int) {
            if (src == dst) return send(distance)
            val used2 = used + src
            for ((next, weight) in gr[src] ?: return) if (next !in used) launch { go(next, used2, distance + weight) }
        }
        go(src, emptySet(), 0)
    }.fold(0, ::maxOf)

    @Suppress("CyclomaticComplexMethod")
    private fun mkGraph(simplify: Boolean): Map<IntPair, Map<IntPair, Int>> = buildMap<_, MutableMap<IntPair, Int>> {
        val lines = if (simplify) { input.replace(slopes, ".") } else { input }.lines()
        for ((y, line) in lines.withIndex()) {
            for ((x, char) in line.withIndex()) {
                this[IntPair(y, x)] = when (char) {
                    '.' -> mutableMapOf<IntPair, Int>().apply {
                        if (lines.getOrNull(y - 1)?.getOrNull(x)?.let(".<^>"::contains) == true) {
                            this[IntPair(y - 1, x)] = 1
                        }
                        if (lines.getOrNull(y + 1)?.getOrNull(x)?.let(".<v>"::contains) == true) {
                            this[IntPair(y + 1, x)] = 1
                        }
                        if (line.getOrNull(x - 1)?.let(".^<v"::contains) == true) this[IntPair(y, x - 1)] = 1
                        if (line.getOrNull(x + 1)?.let(".^>v"::contains) == true) this[IntPair(y, x + 1)] = 1
                    }

                    '^' -> mutableMapOf(IntPair(y - 1, x) to 1)
                    'v' -> mutableMapOf(IntPair(y + 1, x) to 1)
                    '<' -> mutableMapOf(IntPair(y, x - 1) to 1)
                    '>' -> mutableMapOf(IntPair(y, x + 1) to 1)
                    else -> continue
                }
            }
        }

        if (simplify) {
            entries.removeAll { (key, value) ->
                if (value.size != 2) return@removeAll false
                val iterator = value.iterator()
                val (key1, value1) = iterator.next()
                val (key2, value2) = iterator.next()
                val map1 = getValue(key1)
                map1.remove(key)?.also { map1[key2] = it + value2 }
                val map2 = getValue(key2)
                map2.remove(key)?.also { map2[key1] = it + value1 }
                true
            }
        }
    }

    companion object {
        private val slopes = """[<>^v]""".toRegex()
    }
}
