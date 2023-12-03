"""
Day 3: Gear Ratios
"""

import re
from collections import defaultdict
from math import prod

SAMPLE_INPUT = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

NUMBER_RE = re.compile(r"\d+")
SYMBOL_RE = re.compile(r"[^\s\d.]")


def _parse(data):
    lines = data.splitlines()
    for y, line in enumerate(lines):
        for match in NUMBER_RE.finditer(line):
            number = int(match.group(0))
            x0, x1 = match.span()
            for yy in range(max(y - 1, 0), min(y + 2, len(lines))):
                for match in SYMBOL_RE.finditer(lines[yy], x0 - 1, x1 + 1):
                    yield match.start(), yy, number


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    4361
    """
    return sum(number for _, _, number in _parse(data))


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    467835
    """
    gears = defaultdict(list)
    for x, y, number in _parse(data):
        gears[x, y].append(number)
    return sum(prod(numbers) for numbers in gears.values() if len(numbers) == 2)


parts = (part1, part2)
