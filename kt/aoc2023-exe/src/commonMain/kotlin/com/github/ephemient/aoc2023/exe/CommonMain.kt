package com.github.ephemient.aoc2023.exe

import com.github.ephemient.aoc2023.days

internal suspend fun mainImpl(args: Array<out String>) {
    for (day in days) {
        if ((args.isNotEmpty() || day.skipByDefault) && day.name !in args) continue
        println("Day ${day.name}")
        for (part in day.solver(getDayInput(day.day))) println(part())
        println()
    }
}
