"""
Day 10: Pipe Maze
"""

import heapq
import itertools

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

_DIRECTIONS = {
    "|": "ud",
    "-": "lr",
    "L": "ur",
    "J": "ul",
    "7": "ld",
    "F": "rd",
    "S": "udlr",
}


def _parse(data):
    maze = data.splitlines()
    return (
        {
            (y, x): c
            for y, line in enumerate(maze)
            for x, c in enumerate(line)
            if c in _DIRECTIONS
        },
        max(len(line) for line in maze),
        len(maze),
    )


def _part1(maze, width, height):
    (start,) = (position for position, c in maze.items() if c == "S")
    queue, last_d, visited = [(0, start)], -1, set()
    while queue:
        d, (y, x) = heapq.heappop(queue)
        if (y, x) in visited or (y, x) not in maze:
            continue
        visited.add((y, x))
        last_d = d
        directions, neighbors = _DIRECTIONS[maze[(y, x)]], []
        for direction in directions:
            match direction:
                case "u":
                    neighbors.append((y - 1, x))
                case "l":
                    neighbors.append((y, x - 1))
                case "d":
                    neighbors.append((y + 1, x))
                case "r":
                    neighbors.append((y, x + 1))
        for y, x in neighbors:
            if 0 <= y < height and 0 <= x < width and (y, x) in maze:
                heapq.heappush(queue, (d + 1, (y, x)))
    return last_d, visited


def part1(data):
    """
    >>> part1(SAMPLE_INPUT_1)
    4
    >>> part1(SAMPLE_INPUT_2)
    8
    """
    maze, width, height = _parse(data)
    last_d, _ = _part1(maze, width, height)
    return last_d


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
    maze, width, height = _parse(data)
    _, loop = _part1(maze, width, height)
    visited = set()
    for position in itertools.chain(
        ((0, x) for x in range(width + 1)),
        ((y, 0) for y in range(height + 1)),
        ((height, x) for x in range(width + 1)),
        ((y, width) for y in range(height + 1)),
    ):
        stack = [position]
        while stack:
            position = stack.pop()
            if position in visited:
                continue
            visited.add(position)
            y, x = position
            ul = maze.get((y - 1, x - 1), ".") if (y - 1, x - 1) in loop else "."
            ur = maze.get((y - 1, x), ".") if (y - 1, x) in loop else "."
            dl = maze.get((y, x - 1), ".") if (y, x - 1) in loop else "."
            dr = maze.get((y, x), ".") if (y, x) in loop else "."
            if y > 0 and ul in "|J7." and ur in "|LF.":
                stack.append((y - 1, x))
            if x > 0 and ul in "-LJ." and dl in "-7F.":
                stack.append((y, x - 1))
            if y < height and dl in "|J7." and dr in "|LF.":
                stack.append((y + 1, x))
            if x < width and ur in "-LJ." and dr in "-7F.":
                stack.append((y, x + 1))
    visited = {(y, x) for y, x in visited if (y, x + 1) in visited}
    visited = {(y, x) for y, x in visited if (y + 1, x) in visited}
    assert not visited.intersection(loop)
    return width * height - len(loop) - len(visited)


parts = (part1, part2)
