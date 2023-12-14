package com.github.ephemient.aoc2023

fun Iterable<String>.transpose(): List<String> = buildList {
    val strings = this@transpose.filterTo(mutableListOf()) { it.isNotEmpty() }
    var i = 0
    while (strings.isNotEmpty()) {
        add(buildString(strings.size) { for (string in strings) append(string[i]) })
        i++
        strings.removeAll { it.length == i }
    }
}
