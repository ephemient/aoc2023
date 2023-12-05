package com.github.ephemient.aoc2023.web.common

fun interface Solver {
    fun solveDayPart(index: Int, part: Int, input: String): String
}
