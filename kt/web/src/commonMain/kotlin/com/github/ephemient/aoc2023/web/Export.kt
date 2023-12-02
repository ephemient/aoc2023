package com.github.ephemient.aoc2023.web

import com.github.ephemient.aoc2023.days
import kotlin.js.ExperimentalJsExport
import kotlin.js.JsExport

@OptIn(ExperimentalJsExport::class)
@JsExport
fun getDayCount(): Int = days.size

@OptIn(ExperimentalJsExport::class)
@JsExport
fun getDayName(day: Int): String = days[day].name

@OptIn(ExperimentalJsExport::class)
@JsExport
fun solveDayPart(day: Int, part: Int, input: String): String? {
    val block = days[day].solver(input).getOrNull(part) ?: return null
    return block().toString()
}
