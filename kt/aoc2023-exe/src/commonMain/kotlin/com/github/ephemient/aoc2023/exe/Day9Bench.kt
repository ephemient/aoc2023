package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day9
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day9Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(9)
    }

    @Benchmark
    fun part1() = Day9(input).part1()

    @Benchmark
    fun part2() = Day9(input).part2()
}
