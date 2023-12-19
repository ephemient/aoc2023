package com.github.ephemient.aoc2023

class Day19(input: String) {
    private val rules: Map<String, List<Pair<String, Comparison?>>>
    private val points: List<Map<Char, Int>>

    init {
        val iterator = input.lineSequence().iterator()
        rules = buildMap {
            @Suppress("LoopWithTooManyJumpStatements")
            for (line in iterator) {
                if (line.isEmpty() && this.isNotEmpty()) break
                if ('{' !in line || !line.endsWith('}')) continue
                val name = line.substringBefore('{')
                this[name] = line.substring(name.length + 1, line.lastIndex).split(',').map {
                    it.substringAfter(':') to when (it.getOrNull(1)) {
                        '<' -> Comparison.LessThan(it[0], it.substringBefore(':').drop(2).toInt())
                        '>' -> Comparison.GreaterThan(it[0], it.substringBefore(':').drop(2).toInt())
                        else -> null
                    }
                }
            }
        }
        points = buildList {
            for (line in iterator) {
                if (!line.startsWith('{') || !line.endsWith('}')) continue
                this += line.substring(1, line.lastIndex).split(',').filter { it[1] == '=' }.associateBy(
                    keySelector = { it[0] },
                    valueTransform = { it.drop(2).toInt() },
                )
            }
        }
    }

    fun part1(): Int = points.sumOf { point ->
        if ("A" in generateSequence("in") { name ->
                rules[name]?.firstOrNull { (_, it) -> it == null || point[it.key] in it.range }?.first
            }
        ) {
            point.values.sum()
        } else {
            0
        }
    }

    fun part2(): Long = DeepRecursiveFunction<Pair<String, Map<Char, IntRange>>, Long> { (name, bounds) ->
        if (name == "A") return@DeepRecursiveFunction bounds.values.fold(1L) { acc, range -> acc * range.size }
        val list = rules[name] ?: return@DeepRecursiveFunction 0
        val updatedBounds = bounds.toMutableMap()
        var acc = 0L
        for ((next, comparison) in list) {
            acc += callRecursive(next to updatedBounds * comparison)
            updatedBounds -= comparison
        }
        acc
    }("in" to mapOf('x' to 1..4000, 'm' to 1..4000, 'a' to 1..4000, 's' to 1..4000))

    private sealed interface Comparison {
        val key: Char
        val value: Int
        val range: IntRange
        val antirange: IntRange

        data class LessThan(override val key: Char, override val value: Int) : Comparison {
            override val range: IntRange
                get() = Int.MIN_VALUE..<value
            override val antirange: IntRange
                get() = value..Int.MAX_VALUE
        }

        data class GreaterThan(override val key: Char, override val value: Int) : Comparison {
            override val range: IntRange
                get() = value + 1..Int.MAX_VALUE
            override val antirange: IntRange
                get() = Int.MIN_VALUE..value
        }
    }

    companion object {
        private val IntRange.size: Int
            get() = if (isEmpty()) 0 else last - first + 1

        private operator fun Map<Char, IntRange>.times(comparison: Comparison?): Map<Char, IntRange> {
            val key = comparison?.key ?: return this
            val a = this.getOrElse(key) { Int.MIN_VALUE..Int.MAX_VALUE }
            val b = comparison.range
            return this + (key to maxOf(a.first, b.first)..minOf(a.last, b.last))
        }

        private operator fun MutableMap<Char, IntRange>.minusAssign(comparison: Comparison?) {
            val key = comparison?.key ?: return
            val a = this.getOrElse(key) { Int.MIN_VALUE..Int.MAX_VALUE }
            val b = comparison.antirange
            this[key] = maxOf(a.first, b.first)..minOf(a.last, b.last)
        }
    }
}
