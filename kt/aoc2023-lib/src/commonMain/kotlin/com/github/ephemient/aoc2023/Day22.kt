package com.github.ephemient.aoc2023

import kotlinx.coroutines.flow.channelFlow
import kotlinx.coroutines.flow.fold
import kotlinx.coroutines.launch

class Day22(input: String) {
    private val size: Int
    private val rdeps: Map<Int, Set<Int>>
    private val deps: Map<Int, Set<Int>>

    init {
        val bricks = input.lines().mapNotNullTo(mutableListOf()) { line ->
            val components = line.split(',', '~', limit = 6)
                .also { if (it.size != 6) return@mapNotNullTo null }
                .map { it.toIntOrNull() ?: return@mapNotNullTo null }
            Triple(components[0], components[1], components[2]) to Triple(components[3], components[4], components[5])
        }
        this.size = bricks.size
        bricks.sortBy { it.first.third }
        val iterator = bricks.listIterator()
        val zs = mutableMapOf<IntPair, Int>()
        for (brick in iterator) {
            val z0 = (brick.first.first..brick.second.first).maxOf { x ->
                (brick.first.second..brick.second.second).maxOf { y ->
                    zs[IntPair(x, y)] ?: 0
                }
            } + 1
            val z1 = z0 - brick.first.third + brick.second.third
            for (x in brick.first.first..brick.second.first) {
                for (y in brick.first.second..brick.second.second) {
                    zs[IntPair(x, y)] = z1
                }
            }
            iterator.set(brick.first.copy(third = z0) to brick.second.copy(third = z1))
        }
        bricks.sortWith(compareBy({ it.first.third }, { it.second.third }))
        val rdeps = mutableMapOf<Int, MutableSet<Int>>()
        val deps = mutableMapOf<Int, MutableSet<Int>>()
        for ((i, brick) in bricks.withIndex()) {
            var j = i + 1
            while (j < bricks.size && bricks[j].first.third <= brick.second.third) j++
            while (j < bricks.size && bricks[j].first.third == brick.second.third + 1) {
                @Suppress("ComplexCondition")
                if (
                    brick.first.first <= bricks[j].second.first && bricks[j].first.first <= brick.second.first &&
                    brick.first.second <= bricks[j].second.second && bricks[j].first.second <= brick.second.second
                ) {
                    rdeps.getOrPut(i) { mutableSetOf() }.add(j)
                    deps.getOrPut(j) { mutableSetOf() }.add(i)
                }
                j++
            }
        }
        this.rdeps = rdeps
        this.deps = deps
    }

    fun part1(): Int = size - deps.mapNotNullTo(mutableSetOf()) { it.value.singleOrNull() }.size

    suspend fun part2(): Int = channelFlow {
        for (entry in rdeps) {
            launch {
                val deps = deps.mapValuesTo(mutableMapOf()) { it.value.toMutableSet() }
                send(
                    DeepRecursiveFunction<Pair<Int, Iterable<Int>>, Int> { (below, aboves) ->
                        aboves.sumOf { above ->
                            val belows = deps[above] ?: return@sumOf 0
                            belows.remove(below)
                            if (belows.isNotEmpty()) return@sumOf 0
                            deps.remove(above)
                            1 + callRecursive(above to rdeps[above].orEmpty())
                        }
                    }(entry.toPair())
                )
            }
        }
    }.fold(0, Int::plus)
}
