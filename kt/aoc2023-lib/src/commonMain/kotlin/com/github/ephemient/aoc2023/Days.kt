package com.github.ephemient.aoc2023

val days = listOf(
    Day(1, ::Day1, Day1::part1, Day1::part2),
    Day(2, ::Day2, Day2::part1, Day2::part2),
    Day(3, ::Day3, Day3::part1, Day3::part2),
)

data class Day(
    val day: Int,
    val parts: Int,
    val solver: (String) -> List<() -> Any?>,
    val name: String = day.toString(),
)

fun <T> Day(day: Int, create: (String) -> T, vararg parts: (T) -> Any?, name: String = day.toString()): Day =
    Day(
        day = day,
        parts = parts.size,
        solver = { with(create(it)) { parts.map { fun() = it(this) } } },
        name = name,
    )
