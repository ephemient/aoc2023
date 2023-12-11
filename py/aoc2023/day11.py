"""
Day 11: Cosmic Expansion
"""

from bisect import bisect

SAMPLE_INPUT = """
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    374
    """
    return part2(data, n=2)


def part2(data, n=1000000):
    """
    >>> part2(SAMPLE_INPUT, n=10)
    1030
    >>> part2(SAMPLE_INPUT, n=100)
    8410
    """
    lines = data.splitlines()
    rows = [y for y, line in enumerate(lines) if "#" not in line]
    cols = [
        x
        for x in range(max(len(line) for line in lines))
        if all(x >= len(line) or line[x] != "#" for line in lines)
    ]
    galaxies = [
        (y, x)
        for y, line in enumerate(lines)
        for x, char in enumerate(line)
        if char == "#"
    ]
    return sum(
        y1
        - y0
        + abs(x1 - x0)
        + (n - 1)
        * (
            bisect(rows, y1)
            - bisect(rows, y0)
            + abs(bisect(cols, x1) - bisect(cols, x0))
        )
        for i, (y0, x0) in enumerate(galaxies)
        for y1, x1 in galaxies[i + 1 :]
    )


parts = (part1, part2)
