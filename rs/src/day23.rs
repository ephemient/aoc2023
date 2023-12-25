use std::collections::{BTreeMap, BTreeSet};

pub fn part1(data: &str) -> Option<usize> {
    let grid = data.lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    let graph = grid
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            let grid = &grid;
            row.iter().enumerate().filter_map(move |(x, c)| {
                if [b'.', b'<', b'>', b'^', b'v'].contains(c) {
                    let mut edges = BTreeMap::new();
                    if b".<".contains(c) && x > 0 && b".<".contains(&row[x - 1]) {
                        edges.insert((y, x - 1), 1);
                    }
                    if b".>".contains(c) && x + 1 < row.len() && b".>".contains(&row[x + 1]) {
                        edges.insert((y, x + 1), 1);
                    }
                    if b".^".contains(c) && y > 0 {
                        let row = grid[y - 1];
                        if x < row.len() && b".^".contains(&row[x]) {
                            edges.insert((y - 1, x), 1);
                        }
                    }
                    if b".v".contains(c) && y + 1 < grid.len() {
                        let row = grid[y + 1];
                        if x < row.len() && b".v".contains(&row[x]) {
                            edges.insert((y + 1, x), 1);
                        }
                    }
                    Some(((y, x), edges))
                } else {
                    None
                }
            })
        })
        .collect::<BTreeMap<_, _>>();
    let start = *graph.keys().next()?;
    let end = *graph.keys().next_back()?;

    let mut stack = vec![(start, BTreeSet::from([start]), 0)];
    let mut best = None::<usize>;
    while let Some((node, used, distance)) = stack.pop() {
        if node == end {
            best = Some(best.map_or(distance, |best| best.max(distance)));
        }
        let Some(edges) = graph.get(&node) else {
            continue;
        };
        for (next, weight) in edges {
            let mut used = used.clone();
            if used.insert(*next) {
                stack.push((*next, used, distance + weight));
            }
        }
    }

    best
}

pub fn part2(data: &str) -> Option<usize> {
    part1(&data.replace(|c| "<>^v".contains(c), "."))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
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
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(94), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(154), part2(EXAMPLE));
    }
}
