package com.github.ephemient.aoc2023.web

import com.github.ephemient.aoc2023.days
import kotlinx.browser.document
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.async
import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.launch
import kotlinx.dom.appendText
import kotlinx.dom.clear
import kotlinx.html.dom.append
import kotlinx.html.js.option
import org.w3c.dom.HTMLFormElement
import org.w3c.dom.HTMLPreElement
import org.w3c.dom.HTMLSelectElement
import org.w3c.dom.HTMLTextAreaElement

fun main() {
    val daySelect = document.getElementById("day") as HTMLSelectElement
    val input = document.getElementById("input") as HTMLTextAreaElement
    val output = document.getElementById("output") as HTMLPreElement
    val form = document.getElementById("container") as HTMLFormElement
    daySelect.append {
        for ((i, day) in days.withIndex()) {
            option {
                value = i.toString()
                +"Day ${day.name}"
            }
        }
    }
    val job = SupervisorJob()
    form.onsubmit = { event ->
        val day = days[daySelect.value.toInt()]
        job.cancelChildren()
        output.clear()
        GlobalScope.launch(job) {
            val parts = day.solver(input.value).map { async { it() } }
            for (part in parts) {
                val result = part.await()
                output.appendText("$result\n")
            }
        }
        event.preventDefault()
    }
}
