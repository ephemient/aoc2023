"""
Day 22: Sand Slabs
"""

from collections import defaultdict

SAMPLE_INPUT = """
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
"""


# pylint: disable=too-many-locals,redefined-outer-name
def _parse(data):
    bricks = [
        ([int(num) for num in first], [int(num) for num in second])
        for line in data.splitlines()
        if len(parts := line.split("~", maxsplit=1)) == 2
        and len(first := parts[0].split(",", maxsplit=2)) == 3
        and len(second := parts[1].split(",", maxsplit=2)) == 3
        and all(num.isdecimal() for num in first + second)
    ]
    bricks.sort(key=lambda brick: brick[0][2])
    heights = defaultdict(int)
    for first, second in bricks:
        (x0, y0, z0), (x1, y1, z1) = first, second
        dz = (
            max(heights[(x, y)] for x in range(x0, x1 + 1) for y in range(y0, y1 + 1))
            + 1
            - z0
        )
        first[2], second[2] = z0, z1 = z0 + dz, z1 + dz
        for x in range(x0, x1 + 1):
            for y in range(y0, y1 + 1):
                heights[(x, y)] = z1
    bricks.sort(key=lambda brick: brick[0][2])
    rdeps, deps = defaultdict(set), defaultdict(set)
    for i, ((x0, y0, _), (x1, y1, z1)) in enumerate(bricks):
        for j, ((x2, y2, z2), (x3, y3, _)) in enumerate(bricks[i + 1 :]):
            if z2 <= z1:
                continue
            if z2 > z1 + 1:
                break
            if x0 <= x3 and x2 <= x1 and y0 <= y3 and y2 <= y1:
                rdeps[i].add(i + j + 1)
                deps[i + j + 1].add(i)
            j += 1
    return len(bricks), rdeps, deps


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    5
    """
    n, _, deps = _parse(data)
    return n - len({next(iter(below)) for below in deps.values() if len(below) == 1})


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    7
    """
    _, rdeps, deps = _parse(data)
    acc = 0
    for item in rdeps.items():
        _deps = {above: set(belows) for above, belows in deps.items()}
        stack = [item]
        while stack:
            below, aboves = stack.pop()
            for above in aboves:
                belows = _deps[above]
                if below not in belows:
                    continue
                belows.remove(below)
                if belows:
                    continue
                acc += 1
                stack.append((above, rdeps.get(above, ())))
    return acc


parts = (part1, part2)
