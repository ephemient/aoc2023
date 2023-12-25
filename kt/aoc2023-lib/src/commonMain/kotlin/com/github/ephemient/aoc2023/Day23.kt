package com.github.ephemient.aoc2023

import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.update
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.coroutineScope
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

    suspend fun part1(): Int = Solver(src, dst, mkGraph(true))()

    suspend fun part2(): Int = Solver(src, dst, mkGraph(false))()

    private class Solver(
        private val src: IntPair,
        private val dst: IntPair,
        private val gr: Map<IntPair, Map<IntPair, Int>>,
    ) {
        private val best = atomic(0)

        @Suppress("CyclomaticComplexMethod", "ReturnCount")
        fun CoroutineScope.go(src: IntPair, used: Set<IntPair>, distance: Int) {
            val best = best.value
            if (src == dst) {
                if (distance > best) this@Solver.best.update { maxOf(it, distance) }
                return
            }

            val stack = mutableListOf(src)
            val visited = used.toMutableSet().apply { add(src) }
            var potential = distance
            while (true) {
                val node = stack.removeLastOrNull() ?: break
                var maxWeight = 0
                for ((next, weight) in gr[node] ?: continue) {
                    if (next !in used) maxWeight = maxOf(maxWeight, weight)
                    if (!visited.add(next)) continue
                    stack.add(next)
                }
                potential += maxWeight
            }
            if (potential < best || dst !in visited) return

            val used2 = used + src
            for ((next, weight) in gr[src] ?: return) {
                if (next !in used) launch { go(next, used2, distance + weight) }
            }
        }

        suspend operator fun invoke(): Int {
            coroutineScope { go(src, emptySet(), 0) }
            return best.value
        }
    }

    @Suppress("CyclomaticComplexMethod")
    private fun mkGraph(directed: Boolean): Map<IntPair, Map<IntPair, Int>> =
        buildMap<IntPair, MutableMap<IntPair, Edge>> {
            val lines = if (directed) { input } else { input.replace(slopes, ".") }.lines()
            for ((y, line) in lines.withIndex()) {
                for ((x, c) in line.withIndex()) {
                    val l = line.getOrElse(x - 1) { '#' }
                    val r = line.getOrElse(x + 1) { '#' }
                    val u = lines.getOrElse(y - 1) { "" }.getOrElse(x) { '#' }
                    val d = lines.getOrElse(y + 1) { "" }.getOrElse(x) { '#' }
                    val edges = mutableMapOf(
                        IntPair(y, x - 1) to Edge(1, c in ".<" && l in ".<", c in ".>" && l in ".>"),
                        IntPair(y, x + 1) to Edge(1, c in ".>" && r in ".>", c in ".<" && r in ".<"),
                        IntPair(y - 1, x) to Edge(1, c in ".^" && u in ".^", c in ".v" && u in ".v"),
                        IntPair(y + 1, x) to Edge(1, c in ".v" && d in ".v", c in ".^" && d in ".^"),
                    )
                    edges.values.retainAll { it.forward || it.backward }
                    if (edges.isNotEmpty()) this[IntPair(y, x)] = edges
                }
            }

            while (
                entries.removeAll { (key, value) ->
                    when {
                        key == src || key == dst -> false
                        value.size == 1 -> getValue(value.keys.single()).remove(key) != null
                        value.size == 2 -> {
                            val iterator = value.iterator()
                            val (key1, value1) = iterator.next()
                            val (key2, value2) = iterator.next()
                            val map1 = getValue(key1)
                            map1.remove(key)?.also { map1[key2] = it + value2 }
                            val map2 = getValue(key2)
                            map2.remove(key)?.also { map2[key1] = it + value1 }
                            true
                        }
                        else -> false
                    }
                }
            ) {
                // empty
            }
        }
            .mapValues { buildMap { for ((key, edge) in it.value) if (edge.forward) this[key] = edge.weight } }

    private data class Edge(val weight: Int, val forward: Boolean, val backward: Boolean) {
        operator fun plus(other: Edge): Edge =
            Edge(weight + other.weight, forward and other.forward, backward and other.backward)
    }

    companion object {
        private val slopes = """[<>^v]""".toRegex()
    }
}
