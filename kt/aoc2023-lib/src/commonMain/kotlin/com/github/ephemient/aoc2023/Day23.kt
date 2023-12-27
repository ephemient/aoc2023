package com.github.ephemient.aoc2023

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

    fun part1(): Int = solve(mkGraph(true))

    fun part2(): Int = solve(mkGraph(false))

    private fun solve(gr: Map<IntPair, Map<IntPair, Int>>): Int = solve(gr, src, setOf(src), 0, 0)

    private fun solve(
        gr: Map<IntPair, Map<IntPair, Int>>,
        pos: IntPair,
        used: Set<IntPair>,
        distance: Int,
        best: Int,
    ): Int = if (pos == dst) {
        maxOf(distance, best)
    } else {
        val reachable = mutableSetOf<IntPair>()
        val potential = distance + scan(gr, pos, used, reachable)
        if (potential <= best || dst !in reachable) {
            best
        } else {
            gr[pos].orEmpty().entries.sortedByDescending { it.value }.fold(best) { acc, (next, weight) ->
                if (next in used) acc else solve(gr, next, used + next, distance + weight, acc)
            }
        }
    }

    private fun scan(
        gr: Map<IntPair, Map<IntPair, Int>>,
        pos: IntPair,
        used: Set<IntPair>,
        reachable: MutableSet<IntPair>,
    ): Int {
        val next = gr[pos] ?: return 0
        var greatest = 0
        return next.entries.sumOf { (next, weight) ->
            if (next in used) {
                0
            } else {
                greatest = maxOf(greatest, weight)
                if (reachable.add(next)) scan(gr, next, used, reachable) else 0
            }
        } + greatest
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
