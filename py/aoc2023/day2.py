"""
Day 2: Cube Conundrum
"""

import re
from collections import defaultdict
from math import prod

SAMPLE_INPUT = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

GAME_RE = re.compile(r"Game (\d+): ")
CUBE_RE = re.compile(r"(\d+) (\w+)")


def _parse(data):
    for line in data.splitlines():
        if not (match := GAME_RE.match(line)):
            continue
        game_id = int(match.group(1))
        cubes = defaultdict(int)
        for match in CUBE_RE.finditer(line):
            count = int(match.group(1))
            color = match.group(2)
            cubes[color] = max(cubes[color], count)
        yield game_id, cubes


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    8
    """
    acc = 0
    for game_id, cubes in _parse(data):
        if cubes["red"] <= 12 and cubes["green"] <= 13 and cubes["blue"] <= 14:
            acc += game_id
    return acc


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    2286
    """
    return sum(prod(cubes.values()) for _, cubes in _parse(data))


parts = (part1, part2)
