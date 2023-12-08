"""
Day 8: Haunted Wasteland
"""

import re
from functools import reduce
from itertools import accumulate, repeat
from math import lcm

SAMPLE_INPUT_1, SAMPLE_INPUT_2, SAMPLE_INPUT_3 = (
    """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
""",
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
""",
    """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
""",
)

INSTRUCTIONS = re.compile(r"[LR]+")
NODE = re.compile(r"(\w+) = \((\w+), (\w+)\)")


def _parse(data):
    instructions = INSTRUCTIONS.match(data).group()
    table = {node: (left, right) for node, left, right in NODE.findall(data)}

    def step(node, instruction):
        match instruction:
            case "L":
                return table[node][0]
            case "R":
                return table[node][1]

    return (
        lambda start: reduce(step, instructions, start),
        len(instructions),
        table.keys(),
    )


def part1(data):
    """
    >>> part1(SAMPLE_INPUT_1)
    2
    >>> part1(SAMPLE_INPUT_2)
    6
    """
    step, n, _ = _parse(data)
    return n * next(
        i
        for i, node in enumerate(
            accumulate(repeat(()), lambda node, _: step(node), initial="AAA")
        )
        if node == "ZZZ"
    )


def part2(data):
    """
    >>> part2(SAMPLE_INPUT_3)
    6
    """
    step, n, nodes = _parse(data)

    def find_cycle(start):
        i, end = next(
            (i, node)
            for i, node in enumerate(
                accumulate(repeat(()), lambda node, _: step(node), initial=start)
            )
            if node.endswith("Z")
        )
        assert step(start) == step(end)
        return i

    return n * reduce(lcm, (find_cycle(node) for node in nodes if node.endswith("A")))


parts = (part1, part2)
