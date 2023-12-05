package com.github.ephemient.aoc2023.web.common

import com.github.ephemient.aoc2023.days

object DefaultSolver : Solver {
    override fun solveDayPart(index: Int, part: Int, input: String): String =
        days[index].solver(input)[part]().toString()
}
