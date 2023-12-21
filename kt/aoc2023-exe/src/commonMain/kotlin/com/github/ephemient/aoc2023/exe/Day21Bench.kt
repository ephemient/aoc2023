package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day21
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day21Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(21)
    }

    @Benchmark
    fun part1() = Day21(input).part1()
}
