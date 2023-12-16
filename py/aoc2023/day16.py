"""
Day 16: The Floor Will Be Lava
"""

from enum import Enum
from itertools import chain

SAMPLE_INPUT = r"""
.|<2<\....
|v-v\^....
.v.v.|->>>
.v.v.v^.|.
.v.v.v^...
.v.v.v^..\
.v.v/2\\..
<-2-/vv|..
.|<<<2-|.\
.v//.|.v..
"""


class _Direction(Enum):
    U = "up"
    L = "left"
    D = "down"
    R = "right"


_LUT = {
    (_Direction.U, "/"): (_Direction.R,),
    (_Direction.U, "\\"): (_Direction.L,),
    (_Direction.U, "-"): (_Direction.L, _Direction.R),
    (_Direction.L, "/"): (_Direction.D,),
    (_Direction.L, "\\"): (_Direction.U,),
    (_Direction.L, "|"): (_Direction.D, _Direction.U),
    (_Direction.D, "/"): (_Direction.L,),
    (_Direction.D, "\\"): (_Direction.R,),
    (_Direction.D, "-"): (_Direction.L, _Direction.R),
    (_Direction.R, "/"): (_Direction.U,),
    (_Direction.R, "\\"): (_Direction.D,),
    (_Direction.R, "|"): (_Direction.D, _Direction.U),
}


def _move(y, x, d):
    match d:
        case _Direction.U:
            return y - 1, x
        case _Direction.L:
            return y, x - 1
        case _Direction.D:
            return y + 1, x
        case _Direction.R:
            return y, x + 1


def _fill(data: list[str], y: int, x: int, d: _Direction) -> int:
    stack = [(y, x, d)]
    visited = set(stack)
    while stack:
        y1, x1, d1 = stack.pop()
        for d2 in _LUT.get((d1, data[y1][x1]), (d1,)):
            y2, x2 = _move(y1, x1, d2)
            if (
                0 <= y2 < len(data)
                and 0 <= x2 < len(data[y2])
                and (y2, x2, d2) not in visited
            ):
                stack.append((y2, x2, d2))
                visited.add((y2, x2, d2))
    return len({(y, x) for y, x, _ in visited})


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    46
    """
    return _fill(list(filter(None, data.splitlines())), 0, 0, _Direction.R)


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    51
    """
    data = list(filter(None, data.splitlines()))
    return max(
        _fill(data, y, x, d)
        for y, x, d in chain(
            ((y, 0, _Direction.R) for y in range(len(data))),
            ((0, x, _Direction.D) for x in range(len(data[0]))),
            ((y, len(data[0]) - 1, _Direction.L) for y in range(len(data))),
            ((len(data) - 1, x, _Direction.U) for x in range(len(data[-1]))),
        )
    )


parts = (part1, part2)
