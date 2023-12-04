"""
Day 4: Scratchcards
"""

SAMPLE_INPUT = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""


def _parse(data):
    for line in data.splitlines():
        if not line:
            continue
        line = line[line.index(":") + 1 :]
        left, right = line.split("|", maxsplit=1)
        left = set(left.split())
        right = set(right.split())
        yield len(left & right)


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    13
    """
    return sum(1 << card >> 1 for card in _parse(data))


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    30
    """
    cards = list(_parse(data))
    counts = [1 for _ in cards]
    for i, card in enumerate(cards):
        for j in range(card):
            counts[i + j + 1] += counts[i]
    return sum(counts)


parts = (part1, part2)
