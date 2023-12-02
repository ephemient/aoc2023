package com.github.ephemient.aoc2023

val days = listOf(
    Day(1) { with(Day1(it)) { listOf({ part1() }, { part2() }) } },
    Day(2) { with(Day2(it)) { listOf({ part1() }, { part2() }) } },
)

data class Day(
    val day: Int,
    val name: String = day.toString(),
    val solver: (String) -> List<() -> Any?>,
)
