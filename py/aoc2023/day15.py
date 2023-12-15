"""
Day 15: Lens Library
"""

from functools import reduce

SAMPLE_INPUT = """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""


def _hash(string):
    return reduce(lambda acc, char: 17 * (acc + ord(char)) % 256, string, 0)


def part1(data: str):
    """
    >>> part1(SAMPLE_INPUT)
    1320
    """
    return sum(map(_hash, data.strip().split(",")))


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    145
    """
    buckets = [{} for _ in range(256)]
    for step in data.strip().split(","):
        if step.endswith("-"):
            key = step[:-1]
            buckets[_hash(key)].pop(key, None)
        elif "=" in step:
            key = step[: step.index("=")]
            buckets[_hash(key)][key] = int(step[step.index("=") + 1 :])
    return sum(
        (i + 1) * sum((j + 1) * n for j, n in enumerate(bucket.values()))
        for i, bucket in enumerate(buckets)
    )


parts = (part1, part2)
