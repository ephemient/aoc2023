package com.github.ephemient.aoc2023.exe

import node.process.process

@ExperimentalJsExport
@JsExport
fun main() {
    mainImpl(with(process.argv) { copyOfRange(2, size) })
}
