package com.github.ephemient.aoc2023.web.common

import js.import.import
import js.promise.Promise

class WasmSolver internal constructor(private val lib: WasmJs) : Solver {
    override fun solveDayPart(index: Int, part: Int, input: String): String =
        lib.solveDayPart(index, part, input)
}

@Suppress("FunctionNaming")
fun WasmSolver(path: String): Promise<WasmSolver> =
    import<dynamic>(path).then { WasmSolver(it.default.unsafeCast<WasmJs>()) }
