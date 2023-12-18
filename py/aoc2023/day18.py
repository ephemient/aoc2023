"""
Day 18: Lavaduct Lagoon
"""

SAMPLE_INPUT = """
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"""


def _solve(lines):
    x, y, a, l = 0, 0, 0, 0
    for d, n in lines:
        match d:
            case "0" | "R":
                x += n
                a += y * n
            case "1" | "D":
                y += n
            case "2" | "L":
                x -= n
                a -= y * n
            case "3" | "U":
                y -= n
        l += n
    return abs(a) + l // 2 + 1


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    62
    """
    return _solve(
        (words[0], int(words[1]))
        for line in data.splitlines()
        if len(words := line.split()) >= 2
    )


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    952408144115
    """
    return _solve(
        (line[-2:-1], int(line[-7:-2], base=16))
        for line in data.splitlines()
        if line[-9:-7] == "(#" and line[-1:] == ")"
    )


parts = (part1, part2)
