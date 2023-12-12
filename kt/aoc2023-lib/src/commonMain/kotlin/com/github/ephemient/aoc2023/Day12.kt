package com.github.ephemient.aoc2023

class Day12(input: String) {
    private val input = input.lines().mapNotNull { line ->
        val (lhs, rhs) = line.split(' ', limit = 2).takeIf { it.size == 2 }
            ?: return@mapNotNull null
        lhs to rhs.split(',').map { it.toIntOrNull() ?: return@mapNotNull null }
    }

    fun part1(): Long = input.sumOf(::calculate)

    fun part2(): Long = input.sumOf { (string, runs) ->
        calculate(List(5) { string }.joinToString(",") to List(5) { runs }.flatten())
    }

    companion object {
        private fun calculate(input: Pair<String, List<Int>>): Long {
            val memo = mutableMapOf<Pair<String, List<Int>>, Long>()
            return DeepRecursiveFunction<Pair<String, List<Int>>, Long> { (string, runs) ->
                val trimmed = string.trim('.')
                memo.getOrPut(trimmed to runs) {
                    val m = runs.sum()
                    when {
                        trimmed.count { it == '#' } > m ||
                            m > trimmed.length - trimmed.count { it == '.' } ||
                            m + runs.size - 1 > trimmed.length
                        -> 0
                        trimmed.isEmpty() || runs.isEmpty() -> 1
                        else -> if (
                            trimmed.subSequence(1, runs[0]).all { it != '.' } &&
                            trimmed.getOrNull(runs[0]) != '#'
                        ) { callRecursive(trimmed.drop(runs[0] + 1) to runs.subList(1, runs.size)) } else { 0 } +
                            if (!trimmed.startsWith('#')) { callRecursive(trimmed.drop(1) to runs) } else { 0 }
                    }
                }
            }(input)
        }
    }
}
