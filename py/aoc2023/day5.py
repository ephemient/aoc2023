"""
Day 5:
"""

import re
from functools import reduce
from operator import itemgetter

SAMPLE_INPUT = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

NUMBER_RE = re.compile(r"\d+")


def _parse(data):
    stanzas = data.split("\n\n")
    return [int(m.group(0)) for m in NUMBER_RE.finditer(stanzas[0])], [
        sorted(
            (
                (nums[1], nums[1] + nums[2], nums[0] - nums[1])
                for line in stanza.splitlines()
                if len(nums := [int(m.group(0)) for m in NUMBER_RE.finditer(line)]) == 3
            ),
            key=itemgetter(0),
        )
        for stanza in stanzas[1:]
    ]


def _remap(ranges, mappings):
    for start, end in ranges:
        for start2, end2, offset in mappings:
            if start2 >= end or start >= end2:
                continue
            if start < start2:
                yield start, start2
                start = start2
            end2 = min(end, end2)
            yield start + offset, end2 + offset
            start = end2
        if start < end:
            yield start, end


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    35
    """
    seeds, mappings = _parse(data)
    return min(
        map(itemgetter(0), reduce(_remap, mappings, ((x, x + 1) for x in seeds)))
    )


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    46
    """
    seeds, mappings = _parse(data)
    return min(
        map(
            itemgetter(0),
            reduce(_remap, mappings, ((x, x + y) for x, y in zip(*[iter(seeds)] * 2))),
        )
    )


parts = (part1, part2)
