package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day6
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day6Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(6)
    }

    @Benchmark
    fun part1() = Day6(input).part1()

    @Benchmark
    fun part2() = Day6(input).part2()
}
