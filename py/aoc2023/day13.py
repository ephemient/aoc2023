"""
Day 13: Point of Incidence
"""

SAMPLE_INPUT = """
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"""


def _solve(lines, eq):
    for i in range(1, len(lines)):
        if eq(lines[i - 1 :: -1], lines[i:]):
            return i
    return 0


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    405
    """

    def eq(x, y):
        n = min(len(x), len(y))
        return x[:n] == y[:n]

    return sum(
        100 * _solve(lines := group.strip().splitlines(), eq)
        + _solve(list(zip(*lines)), eq)
        for group in data.split("\n\n")
    )


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    400
    """

    def eq(x, y):
        return sum(c != d for a, b in zip(x, y) for c, d in zip(a, b)) == 1

    return sum(
        100 * _solve(lines := group.strip().splitlines(), eq)
        + _solve(list(zip(*lines)), eq)
        for group in data.split("\n\n")
    )


parts = (part1, part2)
