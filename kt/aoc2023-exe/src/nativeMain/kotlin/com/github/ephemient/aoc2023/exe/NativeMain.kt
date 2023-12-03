package com.github.ephemient.aoc2023.exe

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking

fun main(vararg args: String) = runBlocking(Dispatchers.Default) {
    mainImpl(args)
}
