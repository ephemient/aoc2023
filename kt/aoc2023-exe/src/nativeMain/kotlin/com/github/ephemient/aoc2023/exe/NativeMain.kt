package com.github.ephemient.aoc2023.exe

import kotlinx.coroutines.runBlocking

@Suppress("InjectDispatcher")
fun main(vararg args: String): Unit = runBlocking {
    mainImpl(args)
}
