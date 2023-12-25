package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day25
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day25Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(25)
    }

    @Benchmark
    fun part1() = Day25(input).part1()
}
