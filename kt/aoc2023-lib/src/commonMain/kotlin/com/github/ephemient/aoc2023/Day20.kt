package com.github.ephemient.aoc2023

class Day20(input: String) {
    private val machines: Map<String, Pair<Set<String>, Type?>>
    private val dependencies: Map<String, Set<String>>

    init {
        val machines = mutableMapOf<String, Pair<Set<String>, Type?>>()
        val dependencies = mutableMapOf<String, MutableSet<String>>()

        for (line in input.lines()) {
            val (lhs, rhs) = line.split(" -> ", limit = 2).takeIf { it.size == 2 } ?: continue
            val (type, key) = when {
                lhs.startsWith('%') -> Type.FlipFlop to lhs.drop(1)
                lhs.startsWith('&') -> Type.Conjunction to lhs.drop(1)
                else -> null to lhs
            }
            val dsts = rhs.split(", ").toSet()
            machines[key] = dsts to type
            for (dst in dsts) dependencies.getOrPut(dst) { mutableSetOf() }.add(key)
        }

        this.machines = machines
        this.dependencies = dependencies
    }

    fun part1(): Int {
        val state = machines.mapValues { (key, value) ->
            value.second.createState(dependencies[key] ?: emptySet())
        }
        var x = 0
        var y = 0
        repeat(1000) {
            val queue = ArrayDeque<Triple<String, String, Boolean>>()
            queue.add(Triple("button", "broadcaster", false))
            @Suppress("LoopWithTooManyJumpStatements")
            while (true) {
                val (src, key, value) = queue.removeFirstOrNull() ?: break
                if (value) x++ else y++
                val newValue = state[key]?.onPulse(src, value) ?: continue
                machines.getValue(key).first.mapTo(queue) { Triple(key, it, newValue) }
            }
        }
        return x * y
    }

    @Suppress("CyclomaticComplexMethod", "NestedBlockDepth", "ReturnCount")
    fun part2(): Long? {
        val conjunction = dependencies["rx"]?.singleOrNull()
            ?.takeIf { machines[it]?.second == Type.Conjunction }
            ?: return null
        val subsets = (dependencies[conjunction] ?: emptySet()).associateWith { dst ->
            buildSet {
                val stack = mutableListOf(dst)
                @Suppress("LoopWithTooManyJumpStatements")
                while (true) {
                    val key = stack.removeLastOrNull() ?: break
                    if (!add(key)) continue
                    dependencies[key]?.let { stack.addAll(it) }
                }
            }
        }
        subsets.values.toList().let { values ->
            for (i in values.indices) {
                for (j in i + 1..values.lastIndex) {
                    if (values[i].intersect(values[j]).singleOrNull() != "broadcaster") {
                        return null
                    }
                }
            }
        }
        return subsets.entries.fold(1) { acc: Long, (dst, subset) ->
            val state = machines.filterKeys { it in subset }.mapValues { (key, value) ->
                value.second.createState(dependencies[key] ?: emptySet())
            }
            val seen = mutableMapOf<Map<String, Boolean?>, Int>()
            var hadOutput = false
            while (true) {
                var hasOutput = false
                val queue = ArrayDeque<Triple<String, String, Boolean>>()
                queue.add(Triple("button", "broadcaster", false))
                @Suppress("LoopWithTooManyJumpStatements")
                while (true) {
                    val (src, key, value) = queue.removeFirstOrNull() ?: break
                    val newValue = state[key]?.onPulse(src, value) ?: continue
                    if (newValue && key == dst) hasOutput = true
                    machines.getValue(key).first
                        .filter { it in subset }
                        .mapTo(queue) { Triple(key, it, newValue) }
                }
                val snapshot = state.mapValues { it.value.value }
                if (snapshot in seen) {
                    if (!hadOutput) return null
                    val i = seen.getValue(snapshot)
                    if (i != 0) return null
                    break
                }
                seen[snapshot] = seen.size
                if (hadOutput) return null
                hadOutput = hasOutput
            }
            lcm(acc, seen.size.toLong())
        }
    }

    private sealed interface State {
        object Default : State {
            override val value: Boolean?
                get() = null

            override fun onPulse(key: String, value: Boolean): Boolean = value
        }

        class FlipFlop : State {
            override var value = false
                private set

            override fun onPulse(key: String, value: Boolean): Boolean? = if (value) {
                null
            } else {
                !this.value
            }?.also { this.value = it }
        }

        class Conjunction(keys: Set<String>) : State {
            private val state = keys.associateWithTo(mutableMapOf()) { false }

            override val value: Boolean
                get() = !state.values.all { it }

            override fun onPulse(key: String, value: Boolean): Boolean {
                state[key] = value
                return this.value
            }
        }

        val value: Boolean?

        fun onPulse(key: String, value: Boolean): Boolean?
    }

    private enum class Type {
        FlipFlop, Conjunction,
    }

    companion object {
        private fun Type?.createState(keys: Set<String>): State = when (this) {
            null -> State.Default
            Type.FlipFlop -> State.FlipFlop()
            Type.Conjunction -> State.Conjunction(keys)
        }
    }
}
