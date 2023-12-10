package com.github.ephemient.aoc2023

internal class JvmPriorityQueue<E : Any>(comparator: Comparator<E>) :
    PriorityQueue<E>,
    java.util.PriorityQueue<E>(comparator)

actual fun <E : Any> PriorityQueue(comparator: Comparator<E>): PriorityQueue<E> =
    JvmPriorityQueue(comparator)
