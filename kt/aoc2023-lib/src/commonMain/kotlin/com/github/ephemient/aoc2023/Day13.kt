package com.github.ephemient.aoc2023

class Day13(input: String) {
    private val groups: List<List<String>> = buildList {
        input.lineSequence().plus("").fold(mutableListOf<String>()) { group, line ->
            if (line.isNotEmpty()) return@fold group.apply { add(line) } else if (group.isNotEmpty()) add(group)
            mutableListOf()
        }
    }

    fun part1(): Int = groups.sumOf { group ->
        100 * group.findReflection(::zipEquals) + group.transpose().findReflection(::zipEquals)
    }

    fun part2(): Int = groups.sumOf { group ->
        100 * group.findReflection(::zipAlmostEqual) + group.transpose().findReflection(::zipAlmostEqual)
    }

    companion object {
        private fun Iterable<String>.transpose(): List<String> = buildList {
            val strings = this@transpose.filterTo(mutableListOf()) { it.isNotEmpty() }
            var i = 0
            while (strings.isNotEmpty()) {
                add(buildString(strings.size) { for (string in strings) append(string[i]) })
                i++
                strings.removeAll { it.length == i }
            }
        }

        private fun List<String>.findReflection(eq: List<String>.(List<String>) -> Boolean): Int {
            for (i in 1 until lastIndex) {
                if (subList(0, i).asReversed().eq(subList(i, size))) return i
            }
            return 0
        }

        private fun zipEquals(first: List<String>, second: List<String>): Boolean {
            val n = minOf(first.size, second.size)
            return first.subList(0, n) == second.subList(0, n)
        }

        @Suppress("ReturnCount")
        private fun zipAlmostEqual(first: List<String>, second: List<String>): Boolean {
            var almostEqual = false
            for (i in 0..minOf(first.lastIndex, second.lastIndex)) {
                val a = first[i]
                val b = second[i]
                val delta = (0..maxOf(a.lastIndex, b.lastIndex)).count { a.getOrNull(it) != b.getOrNull(it) }
                if (delta > 1) return false
                if (delta == 1) if (almostEqual) return false else almostEqual = true
            }
            return almostEqual
        }
    }
}
