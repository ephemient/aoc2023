package com.github.ephemient.aoc2023.web

import com.github.ephemient.aoc2023.web.getDayCount as jsGetDayCount
import com.github.ephemient.aoc2023.web.getDayName as jsGetDayName
import com.github.ephemient.aoc2023.web.solveDayPart as jsSolveDayPart

external interface Lib {
    fun getDayCount(): Int
    fun getDayName(day: Int): String
    fun solveDayPart(day: Int, part: Int, input: String): String?
}

object JsLib : Lib {
    override fun getDayCount(): Int = jsGetDayCount()
    override fun getDayName(day: Int): String = jsGetDayName(day)
    override fun solveDayPart(day: Int, part: Int, input: String): String? = jsSolveDayPart(day, part, input)
}
