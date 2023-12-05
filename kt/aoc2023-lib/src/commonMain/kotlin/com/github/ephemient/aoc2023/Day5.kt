package com.github.ephemient.aoc2023

class Day5(input: String) {
    private val seeds: List<Long>
    private val mappingsList: List<List<Mapping>>

    init {
        val stanzas = input.split("\n\n")
        seeds = stanzas[0].split(' ').mapNotNull { it.toLongOrNull() }
        mappingsList = stanzas.drop(1).map { stanza ->
            stanza.lines().mapNotNull { line ->
                val (dest, source, size) = line.split(' ').takeIf { it.size == 3 } ?: return@mapNotNull null
                Mapping(
                    dest.toLongOrNull() ?: return@mapNotNull null,
                    source.toLongOrNull() ?: return@mapNotNull null,
                    size.toLongOrNull() ?: return@mapNotNull null,
                )
            }.sortedBy { it.source }
        }
    }

    private fun solve(ranges: List<LongRange>): Long = ranges.flatMap {
        mappingsList.fold(listOf(it)) { acc, mappings ->
            buildList {
                for (range in acc) {
                    val last = mappings.filter { mapping -> range in mapping }.fold(range.first) { first, mapping ->
                        if (first < mapping.source) add(first until mapping.source)
                        val start = maxOf(first, mapping.source)
                        val end = minOf(range.last + 1, mapping.source + mapping.length)
                        val offset = mapping.dest - mapping.source
                        add(start + offset until end + offset)
                        end
                    }
                    if (last <= range.last) add(last..range.last)
                }
            }
        }
    }.minOf { it.first }

    fun part1(): Long = solve(seeds.map { it..it })

    fun part2(): Long = solve(seeds.chunked(2) { (start, length) -> start until start + length })

    private data class Mapping(val dest: Long, val source: Long, val length: Long) {
        operator fun contains(range: LongRange): Boolean =
            !range.isEmpty() && source <= range.last && range.first < source + length
    }
}
