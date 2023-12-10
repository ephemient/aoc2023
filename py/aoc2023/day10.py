"""
Day 10: Pipe Maze
"""

from enum import Enum
from itertools import chain

(
    SAMPLE_INPUT_1,
    SAMPLE_INPUT_2,
    SAMPLE_INPUT_3,
    SAMPLE_INPUT_4,
    SAMPLE_INPUT_5,
    SAMPLE_INPUT_6,
) = (
    """
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
""",
    """
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
""",
    """
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
""",
    """
..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........
""",
    """
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
""",
    """
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
""",
)


class _Direction(Enum):
    U = "up"
    L = "left"
    D = "down"
    R = "right"


_LUT = {
    (_Direction.U, "|"): _Direction.U,
    (_Direction.U, "7"): _Direction.L,
    (_Direction.U, "F"): _Direction.R,
    (_Direction.L, "-"): _Direction.L,
    (_Direction.L, "F"): _Direction.D,
    (_Direction.L, "L"): _Direction.U,
    (_Direction.D, "|"): _Direction.D,
    (_Direction.D, "L"): _Direction.R,
    (_Direction.D, "J"): _Direction.L,
    (_Direction.R, "-"): _Direction.R,
    (_Direction.R, "J"): _Direction.U,
    (_Direction.R, "7"): _Direction.D,
}


def _move(pos, d):
    y, x = pos
    match d:
        case _Direction.U:
            return y - 1, x
        case _Direction.L:
            return y, x - 1
        case _Direction.D:
            return y + 1, x
        case _Direction.R:
            return y, x + 1


def _part1(maze):
    for y, line in enumerate(maze):
        for x, char in enumerate(line):
            if char != "S":
                continue
            start_pos = y, x
            for d in _Direction:
                pos, path = start_pos, []
                try:
                    while d:
                        path.append(pos)
                        y2, x2 = pos = _move(pos, d)
                        d = _LUT.get((d, maze[y2][x2]))
                except IndexError:
                    continue
                if pos == start_pos:
                    return path
    raise ValueError("No path found")


def part1(data):
    """
    >>> part1(SAMPLE_INPUT_1)
    4
    >>> part1(SAMPLE_INPUT_2)
    8
    """
    return len(_part1(data.splitlines())) // 2


def part2(data):
    """
    >>> part2(SAMPLE_INPUT_3)
    4
    >>> part2(SAMPLE_INPUT_4)
    4
    >>> part2(SAMPLE_INPUT_5)
    8
    >>> part2(SAMPLE_INPUT_6)
    10
    """
    maze = data.splitlines()
    path = _part1(maze)
    return (
        abs(
            sum(
                x0 * y1 - x1 * y0
                for (y0, x0), (y1, x1) in zip(path, chain(path[1:], path))
            )
        )
        - len(path)
    ) // 2 + 1


parts = (part1, part2)
