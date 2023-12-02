"""
Day 1: Trebuchet?!
"""

SAMPLE_INPUT_1 = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
SAMPLE_INPUT_2 = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""


def _solve(data, mapping):
    acc = 0
    for line in data.splitlines():
        subset = {key: value for key, value in mapping.items() if key in line}
        if not subset:
            continue
        # pylint: disable=cell-var-from-loop
        x = min(subset.items(), key=lambda item: line.index(item[0]))[1]
        y = max(subset.items(), key=lambda item: line.rindex(item[0]))[1]
        acc += 10 * x + y
    return acc


DIGITS = {str(d): d for d in range(10)}
EXTENDED_DIGITS = DIGITS | {
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
}


def part1(data):
    """
    >>> part1(SAMPLE_INPUT_1)
    142
    """
    return _solve(data, DIGITS)


def part2(data):
    """
    >>> part2(SAMPLE_INPUT_2)
    281
    """
    return _solve(data, EXTENDED_DIGITS)


parts = (part1, part2)
