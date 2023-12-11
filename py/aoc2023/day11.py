"""
Day 11: Cosmic Expansion
"""

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


def _solve1(data, n):
    c = 0
    for i, a in enumerate(data):
        if not a:
            continue
        d = 0
        for b in data[i + 1 :]:
            d += bool(b) or n
            c += d * a * b
    return c


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
    return _solve1([line.count("#") for line in lines], n) + _solve1(
        [
            sum(line[x : x + 1] == "#" for line in lines)
            for x in range(max(len(line) for line in lines))
        ],
        n,
    )


parts = (part1, part2)
