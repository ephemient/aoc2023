package com.github.ephemient.aoc2023

import kotlinx.coroutines.flow.channelFlow
import kotlinx.coroutines.flow.fold
import kotlinx.coroutines.launch

suspend fun <T> Iterable<T>.parSum(block: (T) -> Long): Long = channelFlow {
    for (value in this@parSum) {
        launch {
            send(block(value))
        }
    }
}.fold(0, Long::plus)
