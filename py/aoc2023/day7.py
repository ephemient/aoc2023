"""
Day 7: Camel Cards
"""

from collections import Counter

SAMPLE_INPUT = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""


# pylint: disable=too-many-return-statements
def _rank(hand: tuple[int]):
    counts = Counter(x for x in hand if x >= 0).most_common(2)
    count0 = counts[0][1] if counts else 0
    count1 = counts[1][1] if len(counts) > 1 else 0
    jokers = sum(x < 0 for x in hand)
    if count0 + jokers >= 5:
        return 6
    if count0 + jokers >= 4:
        return 5
    if count0 + count1 + jokers >= 5:
        return 4
    if count0 + jokers >= 3:
        return 3
    if count0 + count1 + jokers >= 4:
        return 2
    if count0 + jokers >= 2:
        return 1
    return 0


def _solve(cards: str, data: str):
    hands = [
        (_rank(hand := tuple(map(cards.find, words[0]))), hand, int(words[1]))
        for line in data.splitlines()
        if len(words := line.split(maxsplit=1)) == 2
    ]
    return sum((i + 1) * bid for i, (_, _, bid) in enumerate(sorted(hands)))


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    6440
    """
    return _solve("23456789TJQKA", data)


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    5905
    """
    return _solve("23456789TQKA", data)


parts = (part1, part2)
