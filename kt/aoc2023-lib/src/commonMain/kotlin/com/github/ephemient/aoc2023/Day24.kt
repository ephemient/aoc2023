package com.github.ephemient.aoc2023

import kotlin.math.roundToLong

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

    @Suppress("DestructuringDeclarationWithTooManyEntries")
    fun part2(): Long? = hailstones.asSequence().withIndex().flatMap { (i, hailstone0) ->
        val (x0, y0, z0, vx0, vy0, vz0) = hailstone0

        hailstones.asSequence().take(i).withIndex().flatMap { (j, hailstone1) ->
            val x1 = hailstone1.x - x0
            val y1 = hailstone1.y - y0
            val z1 = hailstone1.z - z0
            val vx1 = hailstone1.vx - vx0
            val vy1 = hailstone1.vy - vy0
            val vz1 = hailstone1.vz - vz0

            val px1 = y1 * vz1.toDouble() - z1 * vy1.toDouble()
            val py1 = z1 * vx1.toDouble() - x1 * vz1.toDouble()
            val pz1 = x1 * vy1.toDouble() - y1 * vx1.toDouble()

            hailstones.asSequence().take(j).flatMap { hailstone2 ->
                val x2 = hailstone2.x - x0
                val y2 = hailstone2.y - y0
                val z2 = hailstone2.z - z0
                val vx2 = hailstone2.vx - vx0
                val vy2 = hailstone2.vy - vy0
                val vz2 = hailstone2.vz - vz0

                val px2 = y2 * vz2.toDouble() - z2 * vy2.toDouble()
                val py2 = z2 * vx2.toDouble() - x2 * vz2.toDouble()
                val pz2 = x2 * vy2.toDouble() - y2 * vx2.toDouble()

                val mx = py1 * pz2 - pz1 * py2
                val my = pz1 * px2 - px1 * pz2
                val mz = px1 * py2 - py1 * px2

                val u1 = (y1 * vx1 - x1 * vy1) / (my * vx1 - mx * vy1)
                val u2 = (y2 * vx2 - x2 * vy2) / (my * vx2 - mx * vy2)

                listOf((mx * u1 - x1) / vx1, (my * u1 - y1) / vy1, (mz * u1 - z1) / vz1).flatMap { t1 ->
                    listOf((mx * u2 - x2) / vx2, (my * u2 - y2) / vy2, (mz * u2 - z2) / vz2).map { t2 ->
                        (x0 + y0 + z0 + (mx + my + mz) * (u1 * t2 - u2 * t1) / (t2 - t1))
                    }
                }
            }
        }
    }
        .mapNotNull { if (it.isFinite()) it.roundToLong() else null }
        .groupingBy { it }
        .eachCount()
        .maxByOrNull { it.value }
        ?.key

    private data class Hailstone(val x: Long, val y: Long, val z: Long, val vx: Long, val vy: Long, val vz: Long)

    private data class Line(val m: Double, val b: Double, val range: ClosedFloatingPointRange<Double>)
}
