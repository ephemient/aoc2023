"""
Day 24: Never Tell Me The Odds
"""

SAMPLE_INPUT = """19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""


def _parse(data):
    for line in data.splitlines():
        if not line:
            continue
        pos, vel = line.split("@")
        x, y, z = (int(num.strip()) for num in pos.split(","))
        vx, vy, vz = (int(num.strip()) for num in vel.split(","))
        yield (x, y, z), (vx, vy, vz)


# pylint: disable=too-many-locals
def part1(data, lo=200000000000000, hi=400000000000000):
    """
    >>> part1(SAMPLE_INPUT, lo=7, hi=27)
    2
    """
    lines, acc = [], 0
    for point in _parse(data):
        (x0, y0, _), (vx0, vy0, _) = point
        assert vx0
        m0 = vy0 / vx0
        b0 = y0 - x0 * vy0 / vx0
        for m1, b1, x1, vx1 in lines:
            if m0 == m1:
                assert b0 != b1
                continue
            x = (b0 - b1) / (m1 - m0)
            if (
                lo <= x <= hi
                and (x - x0) * vx0 > 0
                and (x - x1) * vx1 > 0
                and lo <= m0 * x + b0 <= hi
            ):
                acc += 1
        lines.append((m0, b0, x0, vx0))
    return acc


def part2(data):
    """
    >>> part2(SAMPLE_INPUT) # doctest: +SKIP
    47
    """
    _parse(data)
    raise NotImplementedError()


parts = (part1,)
