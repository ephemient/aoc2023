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

    @Suppress("CyclomaticComplexMethod", "DestructuringDeclarationWithTooManyEntries", "LongMethod", "NestedBlockDepth")
    fun part2(): Long {
        while (true) {
            val hailstone0 = hailstones.random()
            val (x0, y0, z0, vx0, vy0, vz0) = hailstone0

            val hailstone1 = hailstones.random()
            val x1 = hailstone1.x - x0
            val y1 = hailstone1.y - y0
            val z1 = hailstone1.z - z0
            val vx1 = hailstone1.vx - vx0
            val vy1 = hailstone1.vy - vy0
            val vz1 = hailstone1.vz - vz0

            val px1 = y1 * vz1.toDouble() - z1 * vy1.toDouble()
            val py1 = z1 * vx1.toDouble() - x1 * vz1.toDouble()
            val pz1 = x1 * vy1.toDouble() - y1 * vx1.toDouble()

            val hailstone2 = hailstones.random()
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

            val t1s = doubleArrayOf((mx * u1 - x1) / vx1, (my * u1 - y1) / vy1, (mz * u1 - z1) / vz1)
            val t2s = doubleArrayOf((mx * u2 - x2) / vx2, (my * u2 - y2) / vy2, (mz * u2 - z2) / vz2)

            for (t1 in t1s) {
                @Suppress("LoopWithTooManyJumpStatements")
                for (t2 in t2s) {
                    val scale = (u1 * t2 - u2 * t1) / (t2 - t1)
                    val x = ((x0 + mx * scale).takeIf { !it.isNaN() } ?: continue).roundToLong()
                    val y = ((y0 + my * scale).takeIf { !it.isNaN() } ?: continue).roundToLong()
                    val z = ((z0 + mz * scale).takeIf { !it.isNaN() } ?: continue).roundToLong()
                    val vxs = doubleArrayOf((x0 + x1 - x) / t1 + vx0 + vx1, (x0 + x2 - x) / t2 + vx0 + vx2)
                    val vys = doubleArrayOf((y0 + y1 - y) / t1 + vy0 + vy1, (y0 + y2 - y) / t2 + vy0 + vy2)
                    val vzs = doubleArrayOf((z0 + z1 - z) / t1 + vz0 + vz1, (z0 + z2 - z) / t2 + vz0 + vz2)
                    @Suppress("NAME_SHADOWING")
                    for (vx in vxs) {
                        val vx = (vx.takeIf { !it.isNaN() } ?: continue).roundToLong()
                        for (vy in vys) {
                            val vy = (vy.takeIf { !it.isNaN() } ?: continue).roundToLong()
                            for (vz in vzs) {
                                val vz = (vz.takeIf { !it.isNaN() } ?: continue).roundToLong()
                                println("$x, $y, $x @ $vx, $vy, $vz")
                                if (
                                    hailstones.all {
                                        (it.x - x) * (it.vy - vy) == (it.y - y) * (it.vx - vx) &&
                                            (it.x - x) * (it.vz - vz) == (it.z - z) * (it.vx - vx) &&
                                            (it.y - y) * (it.vz - vz) == (it.z - z) * (it.vy - vy)
                                    }
                                ) {
                                    return x + y + z
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    private data class Hailstone(val x: Long, val y: Long, val z: Long, val vx: Long, val vy: Long, val vz: Long)

    private data class Line(val m: Double, val b: Double, val range: ClosedFloatingPointRange<Double>)
}
