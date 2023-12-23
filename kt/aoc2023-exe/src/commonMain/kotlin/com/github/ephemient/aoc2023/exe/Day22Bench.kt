package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day22
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day22Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(22)
    }

    @Benchmark
    fun part1() = Day22(input).part1()

    @Suppress("EXPOSED_FUNCTION_RETURN_TYPE")
    @Benchmark
    fun part2() = runSuspend {
        Day22(input).part2()
    }
}
