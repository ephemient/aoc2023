"""
Day 23: A Long Walk
"""

SAMPLE_INPUT = """
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
"""


# pylint: disable=too-many-locals
def _parse(data):
    data = {
        (y, x): c
        for y, line in enumerate(data.splitlines())
        for x, c in enumerate(line)
        if c in ".<>^v"
    }
    start, end = min(data), max(data)
    gr = {
        (y, x): {
            dst: (
                c.strip(".") in f and d.strip(".") in f,
                c.strip(".") in b and d.strip(".") in b,
                1,
            )
            for dst, f, b in zip(
                [(y, x - 1), (y, x + 1), (y - 1, x), (y + 1, x)], "<>^v", "><v^"
            )
            if (d := data.get(dst, "#")) in ".<>^v"
        }
        for (y, x), c in data.items()
    }
    done = False
    while not done:
        done = True
        for key in list(gr):
            if key in (start, end):
                continue
            edges = gr[key]
            if not edges:
                del gr[key]
            elif len(edges) == 1:
                (key2,) = edges
                del gr[key]
                del gr[key2][key]
            elif len(edges) == 2:
                (key1, (f01, b01, w01)), (key2, (f02, b02, w02)) = edges.items()
                f1, b1, w1 = gr[key1][key]
                f2, b2, w2 = gr[key2][key]
                del gr[key]
                del gr[key1][key]
                del gr[key2][key]
                gr[key1][key2] = (f1 & f02, b1 & b02, w1 + w02)
                gr[key2][key1] = (f2 & f01, b2 & b01, w2 + w01)
            else:
                continue
            done = False
    return (
        start,
        end,
        {
            key: weights
            for key, edges in gr.items()
            if (weights := {dst: w for dst, (f, _, w) in edges.items() if f})
        },
    )


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    94
    """
    start, end, gr = _parse(data)

    def go(pos, used, distance, best):
        if pos == end:
            return distance if best is None or distance > best else best

        reachable = {pos}

        def dfs(pos):
            for dst in gr.get(pos, {}):
                if dst not in used and dst not in reachable:
                    reachable.add(dst)
                    dfs(dst)

        dfs(pos)
        potential = distance + sum(
            max((w for dst, w in gr.get(dst, {}).items() if dst not in used), default=0)
            for dst in reachable
        )
        if best is not None and potential <= best or end not in reachable:
            return best

        used = used | {pos}
        for dst, w in gr[pos].items():
            if dst not in used:
                best = go(dst, used, distance + w, best)

        return best

    return go(start, set(), 0, None)


def part2(data):
    """
    >>> part2(SAMPLE_INPUT)
    154
    """
    return part1(data.translate(data.maketrans("<>^v", "....")))


parts = (part1, part2)
