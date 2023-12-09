"""
Day 9: Mirage Maintenance
"""

SAMPLE_INPUT = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""


def _extrapolate(nums):
    c, s, n = 1, 0, len(nums)
    for i, x in enumerate(nums):
        c, s = c * (n - i) // (i + 1), c * x - s
    return s


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    114
    """
    return sum(
        _extrapolate([int(word) for word in line.split() if word])
        for line in data.splitlines()
    )


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    2
    """
    return sum(
        _extrapolate([int(word) for word in line.split() if word][::-1])
        for line in data.splitlines()
    )


parts = (part1, part2)
