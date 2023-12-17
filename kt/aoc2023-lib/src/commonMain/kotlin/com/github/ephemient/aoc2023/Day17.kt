package com.github.ephemient.aoc2023

class Day17(input: String) {
    private val input = input.lines().mapNotNull { it.filter(('1'..'9')::contains).ifEmpty { null } }

    fun part1(): Int? = bfs(
        ok = { true },
        next = { direction, distance ->
            if (distance < 3) listOf(direction - 1, direction + 1, direction) else listOf(direction - 1, direction + 1)
        }
    )

    fun part2(): Int? = bfs(
        ok = { it >= 4 },
        next = { direction, distance ->
            when {
                distance < 4 -> listOf(direction)
                distance < 10 -> listOf(direction - 1, direction + 1, direction)
                else -> listOf(direction - 1, direction + 1)
            }
        },
    )

    private inline fun bfs(
        ok: (distance: Int) -> Boolean,
        next: (direction: Direction, distance: Int) -> Iterable<Direction>,
    ): Int? {
        val visited = mutableSetOf<State>()
        val queue = PriorityQueue<IndexedValue<State>>(compareBy { (cost, state) -> cost - state.y - state.x })
        queue.add(IndexedValue(0, State(0, 0, Direction.R, 0)))
        while (!queue.isEmpty()) {
            val (cost, state) = queue.remove()
            if (state.y == input.lastIndex && state.x == input.last().lastIndex && ok(state.distance)) return cost
            if (!visited.add(state)) continue
            for (direction in next(state.direction, state.distance)) {
                val newState = state.move(direction)
                if (newState.y !in input.indices || newState.x !in input[state.y].indices) continue
                queue.add(IndexedValue(cost + input[newState.y][newState.x].digitToInt(), newState))
            }
        }
        return null
    }

    private enum class Direction {
        U, L, D, R;
        operator fun plus(other: Int): Direction = entries[(ordinal + other).mod(entries.size)]
        operator fun minus(other: Int): Direction = entries[(ordinal - other).mod(entries.size)]
    }

    private data class State(
        val y: Int,
        val x: Int,
        val direction: Direction,
        val distance: Int,
    )

    private fun State.move(direction: Direction): State {
        val y = when (direction) {
            Direction.U -> y - 1
            Direction.D -> y + 1
            else -> y
        }
        val x = when (direction) {
            Direction.L -> x - 1
            Direction.R -> x + 1
            else -> x
        }
        return State(
            y = y,
            x = x,
            direction = direction,
            distance = if (direction == this.direction) distance + 1 else 1,
        )
    }
}
