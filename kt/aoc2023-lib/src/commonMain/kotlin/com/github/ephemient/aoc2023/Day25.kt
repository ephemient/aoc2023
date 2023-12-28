package com.github.ephemient.aoc2023

class Day25(input: String) {
    private val graph = input.lineSequence()
        .map { it.split(':', limit = 2) }
        .filter { it.size == 2 }
        .flatMap { (src, dst) -> dst.splitToSequence(' ').filter(String::isNotEmpty).map { src to it } }
        .flatMap { (src, dst) -> listOf(src to dst, dst to src) }
        .groupBy { it.first }
        .mapValues { (_, values) -> buildSet { values.mapTo(this) { it.second } } }

    fun part1(): Int? = cut(graph, 3)

    companion object {
        @Suppress("CyclomaticComplexMethod")
        private fun <T : Comparable<T>> cut(graph: Map<T, Set<T>>, n: Int): Int? = if (n == 0) {
            val components = mutableListOf<Int>()
            val keys = graph.keys.toMutableSet()
            while (true) {
                val iterator = keys.iterator()
                if (!iterator.hasNext()) break
                val start = iterator.next().also { iterator.remove() }
                var m = 0
                DeepRecursiveFunction { key ->
                    m++
                    graph[key]?.forEach { if (keys.remove(it)) callRecursive(it) }
                }(start)
                components.add(m)
            }
            if (components.size > 1) components.fold(1, Int::times) else null
        } else {
            buildMap {
                for (start in graph.keys) {
                    val queue = ArrayDeque<Pair<T, List<T>>>().apply { add(start to emptyList()) }
                    val visited = mutableSetOf(start)
                    while (true) {
                        val (node, path) = queue.removeFirstOrNull() ?: break
                        path.foldRight(node) { prev, cur ->
                            val key = minOf(prev, cur) to maxOf(prev, cur)
                            this[key] = getOrElse(key) { 0 } + 1
                            prev
                        }
                        val nextPath = path + node
                        graph[node]?.filter(visited::add)?.mapTo(queue) { it to nextPath }
                    }
                }
            }.entries.sortedByDescending { it.value }.firstNotNullOfOrNull { (edge,) ->
                val (a, b) = edge
                val nextGraph = graph.mapValues { (key, value) ->
                    when (key) {
                        a -> value - b
                        b -> value - a
                        else -> value
                    }
                }
                cut(nextGraph, n - 1)
            }
        }
    }
}
