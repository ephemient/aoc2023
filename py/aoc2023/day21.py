"""
Day 21: Step Counter
"""

SAMPLE_INPUT = """
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"""


def _count(grid, start, n):
    frontier, visited, acc = {start}, set(), 0
    for d in range(n):
        if not (d ^ n) & 1:
            acc += len(frontier)
        visited |= frontier
        frontier = {
            (y1, x1)
            for y0, x0 in frontier
            for y1, x1 in [(y0 - 1, x0), (y0, x0 - 1), (y0, x0 + 1), (y0 + 1, x0)]
            if 0 <= y1 < len(grid)
            and 0 <= x1 < len(grid[y1])
            and grid[y1][x1] != "#"
            and (y1, x1) not in visited
        }
    return acc + len(frontier)


def part1(data, n=64):
    """
    >>> part1(SAMPLE_INPUT, n=1)
    2
    >>> part1(SAMPLE_INPUT, n=2)
    4
    >>> part1(SAMPLE_INPUT, n=3)
    6
    >>> part1(SAMPLE_INPUT, n=6)
    16
    """
    grid = [line for line in data.splitlines() if line]
    (start,) = (
        (y, x) for y, line in enumerate(grid) for x, c in enumerate(line) if c == "S"
    )
    return _count(grid, start, n)


def part2(data, n=26501365):
    """
    >>> part2(SAMPLE_INPUT, n=6) # doctest: +SKIP
    16
    >>> part2(SAMPLE_INPUT, n=10) # doctest: +SKIP
    50
    >>> part2(SAMPLE_INPUT, n=50) # doctest: +SKIP
    1594
    >>> part2(SAMPLE_INPUT, n=100) # doctest: +SKIP
    6536
    >>> part2(SAMPLE_INPUT, n=500) # doctest: +SKIP
    167004
    >>> part2(SAMPLE_INPUT, n=1000) # doctest: +SKIP
    668697
    >>> part2(SAMPLE_INPUT, n=5000) # doctest: +SKIP
    16733044
    """
    grid = [line for line in data.splitlines() if line]
    m = len(grid)
    q, r = n // m, n % m
    ((y0, x0),) = (
        (y, x) for y, line in enumerate(grid) for x, c in enumerate(line) if c == "S"
    )
    a, b, c, d = (
        _count(
            [line * (2 * i + 1) for line in grid] * (2 * i + 1),
            (y0 + i * m, x0 + i * m),
            r + i * m,
        )
        for i in range(4)
    )
    assert d == a - 3 * b + 3 * c
    return a + (b - a) * q + (c - 2 * b + a) * (q * (q - 1) // 2)


parts = (part1, part2)
