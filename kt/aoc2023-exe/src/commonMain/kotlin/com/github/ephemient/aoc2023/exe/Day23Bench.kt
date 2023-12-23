package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day23
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day23Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(23)
    }

    @Suppress("EXPOSED_FUNCTION_RETURN_TYPE")
    @Benchmark
    fun part1() = runSuspend {
        Day23(input).part1()
    }

    @Suppress("EXPOSED_FUNCTION_RETURN_TYPE")
    @Benchmark
    fun part2() = runSuspend {
        Day23(input).part2()
    }
}
