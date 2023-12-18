package com.github.ephemient.aoc2023

val days = listOf(
    Day(1, ::Day1, Day1::part1, Day1::part2),
    Day(2, ::Day2, Day2::part1, Day2::part2),
    Day(3, ::Day3, Day3::part1, Day3::part2),
    Day(4, ::Day4, Day4::part1, Day4::part2),
    Day(5, ::Day5, Day5::part1, Day5::part2),
    Day(6, ::Day6, Day6::part1, Day6::part2),
    Day(7, ::Day7, Day7::part1, Day7::part2),
    Day(8, ::Day8, Day8::part1, Day8::part2),
    Day(9, ::Day9, Day9::part1, Day9::part2),
    Day(10, ::Day10, Day10::part1, Day10::part2),
    Day(11, ::Day11, Day11::part1, Day11::part2),
    Day(12, ::Day12, Day12::part1, Day12::part2),
    Day(13, ::Day13, Day13::part1, Day13::part2),
    Day(14, ::Day14, Day14::part1, Day14::part2),
    Day(15, ::Day15, Day15::part1, Day15::part2),
    Day(16, ::Day16, Day16::part1, Day16::part2),
    Day(17, ::Day17, Day17::part1, Day17::part2),
    Day(18, ::Day18, Day18::part1, Day18::part2),
)

data class Day(
    val day: Int,
    val parts: Int,
    val solver: (String) -> List<suspend () -> Any?>,
    val name: String = day.toString(),
)

fun <T> Day(day: Int, create: (String) -> T, vararg parts: suspend (T) -> Any?, name: String = day.toString()): Day =
    Day(
        day = day,
        parts = parts.size,
        solver = { with(create(it)) { parts.map { suspend { it.invoke(this) } } } },
        name = name,
    )
