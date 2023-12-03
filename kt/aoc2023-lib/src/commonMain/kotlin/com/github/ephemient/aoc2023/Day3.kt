package com.github.ephemient.aoc2023

class Day3(input: String) {
    private val parts: Map<Pair<Int, Int>, List<Int>> = mutableMapOf<Pair<Int, Int>, MutableList<Int>>().apply {
        val lines = input.trimEnd().lines()
        for ((y, line) in lines.withIndex()) {
            for (match in NUMBER.findAll(line)) {
                val number = match.value.toInt()
                for (y2 in (y - 1).coerceAtLeast(0)..(y + 1).coerceAtMost(lines.lastIndex)) {
                    val line2 = lines[y2]
                    val x0 = match.range.first - 1
                    val x1 = match.range.last + 1
                    for (x in x0.coerceAtLeast(0)..x1.coerceAtMost(line2.lastIndex)) {
                        if (line2[x].isSymbol()) {
                            getOrPut(x to y2) { mutableListOf() } += number
                        }
                    }
                }
            }
        }
    }

    fun part1(): Int = parts.values.sumOf { it.sum() }

    fun part2(): Int = parts.values.sumOf { if (it.size == 2) it[0] * it[1] else 0 }

    companion object {
        private val NUMBER = """\d+""".toRegex()

        private fun Char.isSymbol(): Boolean = this != '.' && !isWhitespace() && !isDigit()
    }
}
