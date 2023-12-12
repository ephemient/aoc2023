"""
Day 12: Hot Springs
"""

from functools import cache

SAMPLE_INPUT = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""


def _solve(string, runs):
    @cache
    def solve_helper(string, runs):
        m = sum(runs)
        if (
            m < string.count("#")
            or m > len(string) - string.count(".")
            or m + len(runs) - 1 > len(string)
        ):
            return 0
        if not string or not runs:
            return 1
        m = 0
        if "." not in string[0 : runs[0]] and not string[runs[0] :].startswith("#"):
            m += solve_helper(string[runs[0] + 1 :].strip("."), runs[1:])
        if not string.startswith("#"):
            m += solve_helper(string[1:], runs)
        return m

    return solve_helper(string.strip("."), runs)


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


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    525152
    """
    return sum(
        _solve(
            "?".join((words[0],) * 5), tuple(int(x) for x in words[1].split(",")) * 5
        )
        for line in data.splitlines()
        if len(words := line.split(maxsplit=1))
    )


parts = (part1, part2)
