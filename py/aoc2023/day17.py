"""
Day 17: Clumsy Crucible
"""

import heapq
from enum import IntEnum

SAMPLE_INPUT_1, SAMPLE_INPUT_2 = (
    """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
""",
    """111111111111
999999999991
999999999991
999999999991
999999999991
""",
)


class _Direction(IntEnum):
    U = 0
    L = 1
    D = 2
    R = 3

    def __add__(self, other):
        return _Direction((self.value + other) % len(_Direction))

    def __sub__(self, other):
        return _Direction((self.value - other) % len(_Direction))


def _solve(data, ok, turns):
    data = [[int(char) for char in line] for line in data.splitlines() if line]
    queue = [(0, (0, 0, _Direction.R, 0))]
    visited = set()
    while queue:
        cost, state = heapq.heappop(queue)
        y1, x1, direction1, distance1 = state
        if y1 == len(data) - 1 and x1 == len(data[-1]) - 1 and ok(distance1):
            return cost
        if state in visited:
            continue
        visited.add(state)
        for direction2 in turns(direction1, distance1):
            y2, x2 = y1, x1
            match direction2:
                case _Direction.U:
                    y2 -= 1
                case _Direction.L:
                    x2 -= 1
                case _Direction.D:
                    y2 += 1
                case _Direction.R:
                    x2 += 1
            if not (0 <= y2 < len(data) and 0 <= x2 < len(data[y2])):
                continue
            distance2 = distance1 + 1 if direction1 == direction2 else 1
            heapq.heappush(
                queue, (cost + data[y2][x2], (y2, x2, direction2, distance2))
            )
    return None


# pylint: disable=unused-argument
def _part1_ok(distance):
    return True


def _part1_turns(direction, distance):
    turns = [direction - 1, direction + 1]
    if distance < 3:
        turns.append(direction)
    return turns


def part1(data):
    """
    >>> part1(SAMPLE_INPUT_1)
    102
    """
    return _solve(data, _part1_ok, _part1_turns)


def _part2_ok(distance):
    return distance >= 4


def _part2_turns(direction, distance):
    turns = []
    if distance >= 4:
        turns.extend([direction - 1, direction + 1])
    if distance < 10:
        turns.append(direction)
    return turns


def part2(data):
    """
    >>> part2(SAMPLE_INPUT_1)
    94
    >>> part2(SAMPLE_INPUT_2)
    71
    """
    return _solve(data, _part2_ok, _part2_turns)


parts = (part1, part2)
