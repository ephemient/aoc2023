package com.github.ephemient.aoc2023.web

import kotlinx.coroutines.suspendCancellableCoroutine
import web.workers.Worker
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException
import kotlin.js.json

class WorkerSolver(
    private val path: String,
    private val minWorkers: Int = 1,
    private val maxWorkers: Int = minWorkers,
) : AsyncSolver {
    init {
        require(minWorkers in 0..maxWorkers)
    }

    private val workers = MutableList(minWorkers) { Worker(path) }

    override suspend fun solveDayPart(index: Int, part: Int, input: String): String {
        val worker = workers.removeFirstOrNull() ?: Worker(path)
        return suspendCancellableCoroutine { cont ->
            cont.invokeOnCancellation { worker.terminate() }
            worker.onerror = {
                worker.terminate()
                cont.resumeWithException(Exception(it.toString()))
            }
            worker.onmessageerror = {
                worker.terminate()
                cont.resumeWithException(Error(it.toString()))
            }
            worker.onmessage = {
                if (workers.size < maxWorkers) {
                    workers += worker
                } else {
                    worker.terminate()
                }
                cont.resume(it.data as String)
            }
            worker.postMessage(json("index" to index, "part" to part, "input" to input))
        }
    }
}
