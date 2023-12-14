"""
Day 14: Parabolic Reflector Dish
"""

from itertools import islice

SAMPLE_INPUT = """
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
"""


def _tilt(data):
    data = list(map(list, data))
    for x in range(max(map(len, data))):
        y0 = 0
        while y0 < len(data):
            n, y1 = 0, y0
            while y1 < len(data):
                c = data[y1][x]
                if c == "O":
                    n += 1
                elif c == "#":
                    break
                y1 += 1
            for y in range(y0, y1):
                data[y][x] = "O" if y < y0 + n else "."
            y0 = y1 + 1
    return data


def _spin(data):
    for _ in range(3):
        data = list(zip(*_tilt(data)[::-1]))
    return tuple(map("".join, zip(*_tilt(data)[::-1])))


def _load(data):
    return sum((len(line) - i) * line.count("O") for i, line in enumerate(data))


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    136
    """
    return _load(_tilt(filter(None, data.splitlines())))


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    64
    """
    n = 1000000000
    data = tuple(filter(None, data.splitlines()))
    cache = {data: 0}
    for i in range(1, n + 1):
        data = _spin(data)
        if data in cache:
            j = cache[data]
            data = next(islice(cache.keys(), j + (n - i) % (i - j), None))
            break
        cache[data] = i
    return _load(data)


parts = (part1, part2)
