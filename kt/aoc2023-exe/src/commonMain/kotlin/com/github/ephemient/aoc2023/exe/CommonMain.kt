package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.days
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

internal suspend fun mainImpl(vararg args: String): Unit = withContext(Dispatchers.Default) {
    for (day in days) {
        if (args.isNotEmpty() && day.name !in args) continue
        println("Day ${day.name}")
        for (part in day.solver(getDayInput(day.day))) println(part())
        println()
    }
}
