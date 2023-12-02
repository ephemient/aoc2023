"""
Benchmarks
"""

import os
from importlib import metadata
from pathlib import Path


def _make_test_bench(day, part):
    def _test_bench(benchmark):
        with Path(os.environ.get("AOC2023_DATADIR") or ".").joinpath(
            f"day{day}.txt"
        ).open(encoding="utf-8") as file:
            data = file.read()
        benchmark(part, data)

    return _test_bench


def _make_test_benches():
    for entry in metadata.entry_points().select(group="aoc2023.days"):
        day = "".join(c for c in entry.name if c.isdigit())
        for part in entry.load():
            yield f"test_{entry.name}_{part.__name__}_bench", _make_test_bench(
                day, part
            )


globals().update(_make_test_benches())
