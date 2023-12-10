package com.github.ephemient.aoc2023

actual fun <E : Any> PriorityQueue(comparator: Comparator<E>): PriorityQueue<E> =
    CommonPriorityQueue(comparator)
