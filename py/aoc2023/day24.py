"""
Day 24: Never Tell Me The Odds
"""

from fractions import Fraction

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
    >>> part2(SAMPLE_INPUT)
    47
    """
    points = list(_parse(data))
    for i, ((x0, y0, z0), (vx0, vy0, vz0)) in enumerate(points):
        # pylint: disable=cell-var-from-loop
        def offset(point):
            (x1, y1, z1), (vx1, vy1, vz1) = point
            return (x1 - x0, y1 - y0, z1 - z0), (vx1 - vx0, vy1 - vy0, vz1 - vz0)

        for j, ((x1, y1, z1), (vx1, vy1, vz1)) in enumerate(map(offset, points[:i])):
            px1 = y1 * vz1 - z1 * vy1
            py1 = z1 * vx1 - x1 * vz1
            pz1 = x1 * vy1 - y1 * vx1

            for (x2, y2, z2), (vx2, vy2, vz2) in map(offset, points[:j]):
                px2 = y2 * vz2 - z2 * vy2
                py2 = z2 * vx2 - x2 * vz2
                pz2 = x2 * vy2 - y2 * vx2

                mx = py1 * pz2 - pz1 * py2
                my = pz1 * px2 - px1 * pz2
                mz = px1 * py2 - py1 * px2

                if my * vx1 == mx * vy1 or my * vx2 == mx * vy2:
                    continue

                u1 = Fraction(y1 * vx1 - x1 * vy1, my * vx1 - mx * vy1)
                u2 = Fraction(y2 * vx2 - x2 * vy2, my * vx2 - mx * vy2)
                t1, t2 = (
                    next(
                        (m * u - p) / v
                        for m, p, v in zip((mx, my, mz), (x, y, z), (vx, vy, vz))
                        if v
                    )
                    for u, x, y, z, vx, vy, vz in (
                        (u1, x1, y1, z1, vx1, vy1, vz1),
                        (u2, x2, y2, z2, vx2, vy2, vz2),
                    )
                )
                return int(
                    x0 + y0 + z0 + (mx + my + mz) * (u1 * t2 - u2 * t1) / (t2 - t1)
                )

    return None


parts = (part1, part2)
