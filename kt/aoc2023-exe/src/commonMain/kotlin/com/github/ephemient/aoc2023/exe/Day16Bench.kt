package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day16
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day16Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(16)
    }

    @Benchmark
    fun part1() = Day16(input).part1()

    @Suppress("EXPOSED_FUNCTION_RETURN_TYPE")
    @Benchmark
    fun part2() = runSuspend {
        Day16(input).part2()
    }
}
