"""
Day 12: Hot Springs
"""

import multiprocessing

SAMPLE_INPUT = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""


def _solve(string, runs):
    counts = [
        i + runs[-1] <= len(string)
        and string[i - 1 : i] != "#"
        and "." not in string[i : i + runs[-1]]
        and "#" not in string[i + runs[-1] :]
        for i in range(len(string))
    ]
    for run in runs[-2::-1]:
        counts = [
            i + run < len(string)
            and string[i - 1 : i] != "#"
            and "." not in string[i : i + run]
            and "#" not in string[i + run : i + run + 1]
            and sum(
                counts[
                    i + run + 1 : string.index("#", i + run + 1) + 1
                    if "#" in string[i + run + 1 :]
                    else len(string)
                ]
            )
            for i in range(len(string))
        ]
    total = sum(counts[: string.index("#") + 1 if "#" in string else len(string)])
    return total


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    21
    """
    return sum(
        _solve(words[0], tuple(int(x) for x in words[1].split(",")))
        for line in data.splitlines()
        if len(words := line.split(maxsplit=1))
    )


def _solve2(words):
    return _solve(
        "?".join((words[0],) * 5), tuple(int(run) for run in words[1].split(",")) * 5
    )


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    525152
    """
    with multiprocessing.Pool() as p:
        return sum(
            p.imap_unordered(
                _solve2,
                (
                    words
                    for line in data.splitlines()
                    if len(words := line.split(maxsplit=1)) == 2
                ),
            )
        )


parts = (part1, part2)
