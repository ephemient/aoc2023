package com.github.ephemient.aoc2023

interface PriorityQueue<E : Any> : Iterable<E> {
    fun isEmpty(): Boolean

    fun add(element: E): Boolean

    @Throws(NoSuchElementException::class)
    fun remove(): E
}

expect fun <E : Any> PriorityQueue(comparator: Comparator<E>): PriorityQueue<E>
