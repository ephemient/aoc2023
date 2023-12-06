"""
Day 6: Wait For It
"""

import re
from math import ceil, floor, prod, sqrt

SAMPLE_INPUT = """
Time:      7  15   30
Distance:  9  40  200
"""

NUMBER = re.compile(r"\d+")


def _wincount(time, distance):
    b = time / 2
    d = sqrt(b * b - distance)
    return ceil(b + d - 1) - floor(b - d + 1) + 1


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    288
    """
    line1, line2 = filter(None, data.splitlines())
    return prod(
        _wincount(int(time.group()), int(distance.group()))
        for time, distance in zip(NUMBER.finditer(line1), NUMBER.finditer(line2))
    )


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    71503
    """
    line1, line2 = filter(None, data.splitlines())
    return _wincount(
        int("".join(NUMBER.findall(line1))), int("".join(NUMBER.findall(line2)))
    )


parts = (part1, part2)
