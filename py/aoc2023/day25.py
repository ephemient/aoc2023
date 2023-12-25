"""
Day 25: Snoverload
"""

import math

import networkx as nx

SAMPLE_INPUT = """
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
"""


def part1(data):
    """
    >>> part1(SAMPLE_INPUT)
    54
    """
    gr = nx.Graph()
    for line in data.splitlines():
        if not line:
            continue
        src, dsts = line.split(":", maxsplit=1)
        gr.add_edges_from((src, dst) for dst in dsts.split())
    gr.remove_edges_from(nx.minimum_edge_cut(gr))
    return math.prod(
        len(component) for component in nx.components.connected_components(gr)
    )


parts = (part1,)
