package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day18
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day18Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(18)
    }

    @Benchmark
    fun part1() = Day18(input).part1()

    @Benchmark
    fun part2() = Day18(input).part2()
}
