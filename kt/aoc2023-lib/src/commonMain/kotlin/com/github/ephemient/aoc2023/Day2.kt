package com.github.ephemient.aoc2023

class Day2(input: String) {
    private val games: List<IndexedValue<Map<String, Int>>> = input.lines().mapNotNull { line ->
        IndexedValue(
            (gameRegex.matchAt(line, 0)?.groups?.get(1) ?: return@mapNotNull null).value.toInt(),
            cubeRegex.findAll(line)
                .groupingBy { it.groupValues[2] }
                .aggregate { _, acc, match, _ -> maxOf(acc ?: 0, match.groupValues[1].toInt()) },
        )
    }

    fun part1(): Int = games.filter { (_, cubes) ->
        cubes.getOrElse("red") { 0 } <= 12 &&
            cubes.getOrElse("green") { 0 } <= 13 &&
            cubes.getOrElse("blue") { 0 } <= 14
    }.sumOf { it.index }

    fun part2(): Int = games.sumOf { it.value.values.fold(1, Int::times) }

    companion object {
        private val gameRegex = """Game (\d+): """.toRegex()
        private val cubeRegex = """(\d+) (\w+)""".toRegex()
    }
}
