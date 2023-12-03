package com.github.ephemient.aoc2023.web

import com.github.ephemient.aoc2023.days
import kotlin.js.ExperimentalJsExport
import kotlin.js.JsExport

@OptIn(ExperimentalJsExport::class)
@JsExport
fun getDayCount(): Int = days.size

@OptIn(ExperimentalJsExport::class)
@JsExport
fun getDayParts(index: Int): Int = days[index].parts

@OptIn(ExperimentalJsExport::class)
@JsExport
fun solveDayPart(index: Int, part: Int, input: String): String = days[index].solver(input)[part]().toString()

@OptIn(ExperimentalJsExport::class)
@JsExport
fun getDayName(index: Int): String = days[index].name
