package com.github.ephemient.aoc2023.exe

import node.process.process

suspend fun main() {
    mainImpl(with(process.argv) { copyOfRange(2, size) })
}
