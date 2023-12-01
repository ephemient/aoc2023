package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.Day1
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State
import kotlinx.coroutines.runBlocking

@State(Scope.Benchmark)
class Day1Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = runBlocking {
            getDayInput(1)
        }
    }

    @Benchmark
    fun part1() = Day1(input).part1()

    @Benchmark
    fun part2() = Day1(input).part2()
}
