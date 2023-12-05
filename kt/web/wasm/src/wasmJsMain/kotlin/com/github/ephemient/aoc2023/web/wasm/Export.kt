package com.github.ephemient.aoc2023.web.wasm

import com.github.ephemient.aoc2023.days

@ExperimentalJsExport
@JsExport
fun solveDayPart(index: Int, part: Int, input: String): String = days[index].solver(input)[part]().toString()
