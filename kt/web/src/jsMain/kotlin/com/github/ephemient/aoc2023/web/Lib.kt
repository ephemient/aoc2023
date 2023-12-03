package com.github.ephemient.aoc2023.web

import com.github.ephemient.aoc2023.web.getDayCount as jsGetDayCount
import com.github.ephemient.aoc2023.web.getDayName as jsGetDayName
import com.github.ephemient.aoc2023.web.getDayParts as jsGetDayParts
import com.github.ephemient.aoc2023.web.solveDayPart as jsSolveDayPart

external interface Lib {
    fun getDayCount(): Int
    fun getDayParts(index: Int): Int
    fun solveDayPart(index: Int, part: Int, input: String): String
    fun getDayName(index: Int): String
}

object JsLib : Lib {
    override fun getDayCount(): Int = jsGetDayCount()
    override fun getDayParts(index: Int): Int = jsGetDayParts(index)
    override fun solveDayPart(index: Int, part: Int, input: String): String = jsSolveDayPart(index, part, input)
    override fun getDayName(index: Int): String = jsGetDayName(index)
}
