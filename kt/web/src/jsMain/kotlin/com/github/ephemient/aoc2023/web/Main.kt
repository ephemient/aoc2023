package com.github.ephemient.aoc2023.web

import com.github.ephemient.aoc2023.days
import com.github.ephemient.aoc2023.web.common.DefaultSolver
import com.github.ephemient.aoc2023.web.common.WasmSolver
import js.promise.Promise
import js.promise.await
import js.promise.catch
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
    val solver = try {
        Promise.resolve(WorkerSolver("worker/worker.js"))
    } catch (e: dynamic) {
        console.error(e)
        WasmSolver("wasm/aoc2023-web-wasm-wasm-js.mjs").catch {
            console.error(it)
            DefaultSolver
        }.then { AsyncSolver(it) }
    }

    val daySelect = document.getElementById("day") as HTMLSelectElement
    val inputField = document.getElementById("input") as HTMLTextAreaElement
    val outputField = document.getElementById("output") as HTMLPreElement
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
        val index = daySelect.value.toInt()
        val input = inputField.value
        job.cancelChildren()
        outputField.clear()
        GlobalScope.launch(job) {
            for (result in Array(days[index].parts) { async { solver.await().solveDayPart(index, it, input) } }) {
                outputField.appendText("${result.await()}\n")
            }
        }
        event.preventDefault()
    }
}
