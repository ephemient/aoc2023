"""
Day 10: Pipe Maze
"""

from enum import Enum

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


class Direction(Enum):
    """
    Cardinal direction.
    """

    U = "up"
    L = "left"
    D = "down"
    R = "right"

    def move(self, pos):
        """
        Move a point by one step in this direction.
        """
        y, x = pos
        match self:
            case Direction.U:
                return y - 1, x
            case Direction.L:
                return y, x - 1
            case Direction.D:
                return y + 1, x
            case Direction.R:
                return y, x + 1

    def __neg__(self):
        match self:
            case Direction.U:
                return Direction.D
            case Direction.L:
                return Direction.R
            case Direction.D:
                return Direction.U
            case Direction.R:
                return Direction.L


_SYMBOLS = {
    "|": (Direction.U, Direction.D),
    "-": (Direction.L, Direction.R),
    "L": (Direction.U, Direction.R),
    "J": (Direction.U, Direction.L),
    "7": (Direction.L, Direction.D),
    "F": (Direction.D, Direction.R),
}


def _part1(maze):
    for y, line in enumerate(maze):
        for x, char in enumerate(line):
            if char != "S":
                continue
            start_pos = y, x
            for start_dir in Direction:
                pos = start_dir.move(start_pos)
                last_dir = -start_dir
                path = [start_pos]
                while pos != start_pos:
                    y, x = pos
                    try:
                        char = maze[y][x]
                    except IndexError:
                        break
                    dirs = _SYMBOLS.get(char, ())
                    if last_dir not in dirs:
                        break
                    path.append(pos)
                    (next_dir,) = (d for d in dirs if d != last_dir)
                    pos = next_dir.move(pos)
                    last_dir = -next_dir
                else:
                    return start_pos, (start_dir, last_dir), path


def part1(data):
    """
    >>> part1(SAMPLE_INPUT_1)
    4
    >>> part1(SAMPLE_INPUT_2)
    8
    """
    return len(_part1(data.splitlines())[2]) // 2


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
    start_pos, start_dirs, path = _part1(maze)
    path = sorted(path)
    count, up, down, path_index = 0, False, False, 0
    for y, line in enumerate(maze):
        for x, char in enumerate(line):
            if path_index < len(path) and path[path_index] == (y, x):
                dirs = start_dirs if start_pos == (y, x) else _SYMBOLS[char]
                path_index += 1
                up ^= Direction.U in dirs
                down ^= Direction.D in dirs
            else:
                if up and down:
                    count += 1
                assert up == down
    return count


parts = (part1, part2)
