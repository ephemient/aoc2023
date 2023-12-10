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


class Day10:
    __doc__ = __doc__

    _DIRECTIONS = {
        "|": "ud",
        "-": "lr",
        "L": "ur",
        "J": "ul",
        "7": "ld",
        "F": "rd",
        "S": "udlr",
    }

    def __init__(self, data):
        maze = data.splitlines()
        self._maze = {
            (y, x): c
            for y, line in enumerate(maze)
            for x, c in enumerate(line)
            if c in Day10._DIRECTIONS
        }
        self._height = len(maze)
        self._width = max(len(line) for line in maze)
        self._loop = None

    def part1(self):
        """
        >>> Day10(SAMPLE_INPUT_1).part1()
        4
        >>> Day10(SAMPLE_INPUT_2).part1()
        8
        """
        (start,) = (position for position, c in self._maze.items() if c == "S")
        queue, last_d, visited = [(0, start)], -1, set()
        while queue:
            d, (y, x) = heapq.heappop(queue)
            if (y, x) in visited or (y, x) not in self._maze:
                continue
            visited.add((y, x))
            last_d = d
            directions, neighbors = Day10._DIRECTIONS[self._maze[(y, x)]], []
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
                if (
                    0 <= y < self._height
                    and 0 <= x < self._width
                    and (y, x) in self._maze
                ):
                    heapq.heappush(queue, (d + 1, (y, x)))
        self._loop = visited
        return last_d

    def part2(self):
        """
        >>> Day10(SAMPLE_INPUT_3).part2()
        4
        >>> Day10(SAMPLE_INPUT_4).part2()
        4
        >>> Day10(SAMPLE_INPUT_5).part2()
        8
        >>> Day10(SAMPLE_INPUT_6).part2()
        10
        """
        if self._loop is None:
            self.part1()
        visited = set()
        for position in itertools.chain(
            ((0, x) for x in range(self._width + 1)),
            ((y, 0) for y in range(self._height + 1)),
            ((self._height, x) for x in range(self._width + 1)),
            ((y, self._width) for y in range(self._height + 1)),
        ):
            stack = [position]
            while stack:
                position = stack.pop()
                if position in visited:
                    continue
                visited.add(position)
                y, x = position
                ul = (
                    self._maze.get((y - 1, x - 1), ".")
                    if (y - 1, x - 1) in self._loop
                    else "."
                )
                ur = (
                    self._maze.get((y - 1, x), ".") if (y - 1, x) in self._loop else "."
                )
                dl = (
                    self._maze.get((y, x - 1), ".") if (y, x - 1) in self._loop else "."
                )
                dr = self._maze.get((y, x), ".") if (y, x) in self._loop else "."
                if y > 0 and ul in "|J7." and ur in "|LF.":
                    stack.append((y - 1, x))
                if x > 0 and ul in "-LJ." and dl in "-7F.":
                    stack.append((y, x - 1))
                if y < self._height and dl in "|J7." and dr in "|LF.":
                    stack.append((y + 1, x))
                if x < self._width and ur in "-LJ." and dr in "-7F.":
                    stack.append((y, x + 1))
        visited = {(y, x) for y, x in visited if (y, x + 1) in visited}
        visited = {(y, x) for y, x in visited if (y + 1, x) in visited}
        assert not visited.intersection(self._loop)
        return self._width * self._height - len(self._loop) - len(visited)


parts = (lambda data: Day10(data).part1(), lambda data: Day10(data).part2())
