package com.github.ephemient.aoc2023

class Day15(input: String) {
    private val input = input.split(',', '\n').filter { it.isNotEmpty() }

    fun part1(): Int = input.sumOf { it.hash() }

    fun part2(): Int {
        val buckets = List(256) { mutableMapOf<String, Int>() }
        for (step in input) {
            val split = step.indexOfAny(splitters)
            val key = step.substring(0 until split)
            if (split == step.lastIndex) {
                buckets[key.hash()].remove(key)
            } else {
                buckets[key.hash()][key] = step.substring(split + 1).toInt()
            }
        }
        return buckets.withIndex().sumOf { (i, bucket) ->
            (i + 1) * bucket.values.withIndex().sumOf { (j, value) -> (j + 1) * value }
        }
    }

    companion object {
        private val splitters = charArrayOf('-', '=')

        private fun String.hash(): Int = fold(0) { acc, char -> 17 * (acc + char.code) } and 255
    }
}
