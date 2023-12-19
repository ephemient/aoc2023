"""
Day 19: Aplenty
"""

from math import prod

SAMPLE_INPUT = """
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"""


def _parse_rules(data):
    for line in data.splitlines():
        if "{" not in line or not line.endswith("}"):
            continue
        yield line[: line.index("{")], [
            (
                rule[rule.index(":") + 1 :],
                (rule[0], rule[1], int(rule[2 : rule.index(":")])),
            )
            if ":" in rule
            else (rule, None)
            for rule in line[line.index("{") + 1 : -1].split(",")
        ]


def _parse_points(data):
    for line in data.splitlines():
        if not line.startswith("{") or not line.endswith("}"):
            continue
        yield {kv[0]: int(kv[2:]) for kv in line[1:-1].split(",") if kv[1] == "="}


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    19114
    """
    rules, points = data.split("\n\n", maxsplit=1)
    rules = dict(_parse_rules(rules))
    points = list(_parse_points(points))

    acc = 0
    for point in points:
        name = "in"
        while name in rules:
            for name, comparison in rules[name]:
                if not comparison:
                    break
                key, compare, value = comparison
                if (
                    compare == "<"
                    and point[key] < value
                    or compare == ">"
                    and point[key] > value
                ):
                    break
            else:
                raise RuntimeError("unreachable")
        if name == "A":
            acc += sum(point.values())
    return acc


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    167409079868000
    """
    rules, _ = data.split("\n\n", maxsplit=1)
    rules = dict(_parse_rules(rules))

    def go(name, bounds):
        if any(first > last for first, last in bounds.values()):
            return 0
        if name == "A":
            return prod(last - first + 1 for first, last in bounds.values())
        if name not in rules:
            return 0
        acc = 0
        # pylint: disable=R1704
        for name, comparison in rules[name]:
            if not comparison:
                acc += go(name, bounds)
                break
            key, compare, value = comparison
            lo, hi = bounds[key]
            match compare:
                case "<":
                    intersection = lo, min(hi, value - 1)
                    difference = value, hi
                case ">":
                    intersection = max(lo, value + 1), hi
                    difference = lo, min(hi, value)
                case _:
                    raise KeyError(compare)
            if difference[0] > difference[1]:
                break
            acc += go(name, bounds | {key: intersection})
            bounds[key] = difference
        return acc

    return go("in", {k: (1, 4000) for k in "xmas"})


parts = (part1, part2)
