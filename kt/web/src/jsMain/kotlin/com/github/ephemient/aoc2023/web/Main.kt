package com.github.ephemient.aoc2023.web

import js.import.import
import js.promise.await
import kotlinx.browser.document
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.launch
import kotlinx.coroutines.yield
import kotlinx.dom.appendText
import kotlinx.dom.clear
import kotlinx.html.dom.append
import kotlinx.html.js.option
import org.w3c.dom.HTMLFormElement
import org.w3c.dom.HTMLPreElement
import org.w3c.dom.HTMLSelectElement
import org.w3c.dom.HTMLTextAreaElement

suspend fun main() {
    val lib = try {
        import<dynamic>("./aoc2023-web-wasm-js.mjs").await().default.unsafeCast<Lib>()
    } catch (e: dynamic) {
        console.error(e)
        JsLib
    }

    val daySelect = document.getElementById("day") as HTMLSelectElement
    val inputField = document.getElementById("input") as HTMLTextAreaElement
    val outputField = document.getElementById("output") as HTMLPreElement
    val form = document.getElementById("container") as HTMLFormElement
    daySelect.append {
        repeat(lib.getDayCount()) { i ->
            option {
                value = i.toString()
                +"Day ${lib.getDayName(i)}"
            }
        }
    }
    val job = SupervisorJob()
    form.onsubmit = { event ->
        val day = daySelect.value.toInt()
        val input = inputField.value
        job.cancelChildren()
        outputField.clear()
        GlobalScope.launch(job) {
            var part = 0
            while (true) {
                val result = lib.solveDayPart(day, part++, input) ?: break
                outputField.appendText("$result\n")
                yield()
            }
        }
        event.preventDefault()
    }
}
