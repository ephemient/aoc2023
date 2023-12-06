package com.github.ephemient.aoc2023

class Day6(private val input: String) {
    private val races = input.lines().let { (line1, line2) ->
        NUMBER.findAll(line1).map { it.value.toInt() } zip
            NUMBER.findAll(line2).map { it.value.toInt() }
    }

    fun part1(): Int = races.fold(1) { acc, (time, distance) ->
        acc * winCount(time, distance)
    }

    fun part2(): Int {
        val (time, distance) = NUMBER.findAll(input.replace(" ", "")).map { it.value.toInt() }.toList()
        println(time)
        println(distance)
        return winCount(time, distance)
    }

    companion object {
        private val NUMBER = """\d+""".toRegex()

        private fun winCount(time: Int, distance: Int): Int {
            val lo = run {
                var lo = 0
                var hi = time / 2 + 1
                while (lo < hi) {
                    println("<$lo..$hi")
                    val x = lo + (hi - lo) / 2
                    val y = x * (time - x)
                    if (y <= distance) lo = x + 1 else hi = x
                }
                hi
            }
            val hi = run {
                var lo = time / 2
                var hi = time + 1
                while (lo < hi) {
                    println(">$lo..$hi")
                    val x = lo + (hi - lo) / 2
                    val y = x * (time - x)
                    if (y <= distance) hi = x + 1 else lo = x
                }
                lo
            }
            println("$time/$distance $lo..$hi")
            return hi - lo + 1
        }
    }
}
