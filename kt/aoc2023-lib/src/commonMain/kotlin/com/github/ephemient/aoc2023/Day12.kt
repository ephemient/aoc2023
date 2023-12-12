package com.github.ephemient.aoc2023

class Day12(input: String) {
    private val input = input.lines().mapNotNull { line ->
        val (lhs, rhs) = line.split(' ', limit = 2).takeIf { it.size == 2 }
            ?: return@mapNotNull null
        lhs to rhs.split(',').map { it.toIntOrNull() ?: return@mapNotNull null }
    }

    suspend fun part1(): Long = input.parSum { (string, runs) -> calculate(string, runs).toLong() }

    suspend fun part2(): Long = input.parSum { (string, runs) ->
        calculate(List(5) { string }.joinToString(","), List(5) { runs }.flatten()).toLong()
    }

    companion object {
        private fun calculate(string: String, runs: List<Int>): Int {
            val arr = runs.subList(0, runs.lastIndex).asReversed().fold(
                IntArray(string.length) { i ->
                    if (
                        i + runs.last() > string.length ||
                        string.getOrElse(i - 1) { '.' } == '#' ||
                        (i until i + runs.last()).any { string[it] == '.' } ||
                        (i + runs.last() until string.length).any { string[it] == '#' }
                    ) 0 else 1
                }
            ) { arr, run ->
                IntArray(string.length) { i ->
                    if (i + run > string.length ||
                        string.getOrElse(i - 1) { '.' } == '#' ||
                        (i until (i + run).coerceAtMost(string.length)).any { string[it] == '.' } ||
                        string.getOrElse(i + run) { '.' } == '#'
                    ) return@IntArray 0
                    var acc = 0
                    for (j in i + run + 1 until string.length) {
                        acc += arr[j]
                        if (string[j] == '#') break
                    }
                    acc
                }
            }
            return arr.take(string.indexOf('#') + 1).sum()
        }
    }
}
