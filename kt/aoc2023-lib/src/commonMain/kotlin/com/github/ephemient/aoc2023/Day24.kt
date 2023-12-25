package com.github.ephemient.aoc2023

class Day24(input: String) {
    private val hailstones = input.lines().mapNotNull { line ->
        val (pos, vel) = line.split('@', limit = 2).takeIf { it.size == 2 } ?: return@mapNotNull null
        val (x, y, z) = pos.split(',', limit = 3).takeIf { it.size == 3 } ?: return@mapNotNull null
        val (vx, vy, vz) = vel.split(',', limit = 3).takeIf { it.size == 3 } ?: return@mapNotNull null
        Hailstone(
            x = x.trim().toLongOrNull() ?: return@mapNotNull null,
            y = y.trim().toLongOrNull() ?: return@mapNotNull null,
            z = z.trim().toLongOrNull() ?: return@mapNotNull null,
            vx = vx.trim().toLongOrNull() ?: return@mapNotNull null,
            vy = vy.trim().toLongOrNull() ?: return@mapNotNull null,
            vz = vz.trim().toLongOrNull() ?: return@mapNotNull null,
        )
    }

    fun part1(lo: Double = 200000000000000.0, hi: Double = 400000000000000.0): Int {
        val lines = mutableListOf<Line>()
        return hailstones.sumOf {
            val m0 = it.vy / it.vx.toDouble()
            val b0 = it.y - m0 * it.x
            val range0 = if (it.vx < 0) {
                Double.NEGATIVE_INFINITY..it.x.toDouble()
            } else {
                it.x.toDouble()..Double.POSITIVE_INFINITY
            }
            lines.count { (m1, b1, range1) ->
                if (m0 == m1) return@count false
                val x = (b0 - b1) / (m1 - m0)
                x in lo..hi && m0 * x + b0 in lo..hi && x in range0 && x in range1
            }.also { lines.add(Line(m0, b0, range0)) }
        }
    }

    fun part2(): Long {
        TODO()
    }

    private data class Hailstone(val x: Long, val y: Long, val z: Long, val vx: Long, val vy: Long, val vz: Long)

    private data class Line(val m: Double, val b: Double, val range: ClosedFloatingPointRange<Double>)
}
