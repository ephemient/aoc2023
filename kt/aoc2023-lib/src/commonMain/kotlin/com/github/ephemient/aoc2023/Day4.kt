package com.github.ephemient.aoc2023

class Day4(input: String) {
    private val cards = input.lines().mapNotNull { line ->
        val (left, right) = line.substringAfter(':').split('|', limit = 2).takeIf { it.size == 2 }
            ?: return@mapNotNull null
        (left.trim().split(WHITESPACE).toSet() intersect right.trim().split(WHITESPACE).toSet()).size
    }

    fun part1(): Int = cards.sumOf { 1 shl it shr 1 }

    fun part2(): Int = IntArray(cards.size) { 1 }.also { counts ->
        for ((i, card) in cards.withIndex()) {
            for (j in i + 1..(i + card).coerceAtMost(counts.lastIndex)) {
                counts[j] += counts[i]
            }
        }
    }.sum()

    companion object {
        private val WHITESPACE = """\s+""".toRegex()
    }
}
