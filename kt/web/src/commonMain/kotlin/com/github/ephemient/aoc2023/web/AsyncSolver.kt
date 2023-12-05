package com.github.ephemient.aoc2023.web

import com.github.ephemient.aoc2023.web.common.Solver

fun interface AsyncSolver {
    suspend fun solveDayPart(index: Int, part: Int, input: String): String
}

fun AsyncSolver(solver: Solver): AsyncSolver =
    AsyncSolver { index, part, input -> solver.solveDayPart(index, part, input) }
