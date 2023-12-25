use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};

pub fn part1(data: &str) -> Option<usize> {
    let grid = data.lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    let mut graph = grid
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            let grid = &grid;
            row.iter().enumerate().filter_map(move |(x, c)| {
                let edges = [
                    (b".<", b".>", Some(y), x.checked_sub(1)),
                    (b".>", b".<", Some(y), x.checked_add(1)),
                    (b".^", b".v", y.checked_sub(1), Some(x)),
                    (b".v", b".^", y.checked_add(1), Some(x)),
                ]
                .into_iter()
                .filter_map(|(f, b, y2, x2)| {
                    let (y2, x2) = (y2?, x2?);
                    let d = grid.get(y2)?.get(x2)?;
                    let f = f.contains(c) && f.contains(d);
                    let b = b.contains(c) && b.contains(d);
                    if f || b {
                        Some(((y2, x2), (f, b, 1)))
                    } else {
                        None
                    }
                })
                .collect::<BTreeMap<_, _>>();
                if edges.is_empty() {
                    None
                } else {
                    Some(((y, x), edges))
                }
            })
        })
        .collect::<BTreeMap<_, _>>();
    let start = *graph.keys().next()?;
    let end = *graph.keys().next_back()?;
    loop {
        let mut any_removed = false;
        for key in graph
            .keys()
            .copied()
            .filter(|key| key != &start && key != &end && grid[key.0][key.1] == b'.')
            .collect::<Vec<_>>()
        {
            let Entry::Occupied(mut entry) = graph.entry(key) else {
                panic!("expected Entry::Occupied");
            };
            let edges = entry.get_mut();
            if edges.len() == 1 {
                let &key1 = edges.keys().next().unwrap();
                entry.remove();
                graph.get_mut(&key1).unwrap().remove(&key);
                any_removed = true;
            } else if edges.len() == 2 {
                let mut iter = edges.iter();
                let (&key1, &(f1, b1, w1)) = iter.next().unwrap();
                let (&key2, &(f2, b2, w2)) = iter.next().unwrap();
                entry.remove();
                let edges = graph.get_mut(&key1).unwrap();
                if let Some((f, b, w)) = edges.remove(&key) {
                    edges.insert(key2, (f && f2, b && b2, w + w2));
                }
                let edges = graph.get_mut(&key2).unwrap();
                if let Some((f, b, w)) = edges.remove(&key) {
                    edges.insert(key1, (f && f1, b && b1, w + w1));
                }
                any_removed = true;
            }
        }
        if !any_removed {
            break;
        }
    }
    let graph = graph
        .into_iter()
        .filter_map(|(key, edges)| {
            let edges = edges
                .into_iter()
                .filter_map(|(key, (f, _, w))| if f { Some((key, w)) } else { None })
                .collect::<BTreeMap<_, _>>();
            if edges.is_empty() {
                None
            } else {
                Some((key, edges))
            }
        })
        .collect::<BTreeMap<_, _>>();

    let mut stack = vec![(start, BTreeSet::from([start]), 0)];
    let mut best = None::<usize>;
    while let Some((node, used, distance)) = stack.pop() {
        if node == end {
            best = Some(best.map_or(distance, |best| best.max(distance)));
            continue;
        }
        let mut potential = distance;
        let mut used2 = used.clone();
        let mut stack2 = vec![node];
        while let Some(next) = stack2.pop() {
            let Some(edges) = graph.get(&next) else {
                continue;
            };
            let mut best_weight = 0;
            for (next, weight) in edges {
                if used.contains(next) {
                    continue;
                }
                best_weight = best_weight.max(*weight);
                if used2.insert(*next) {
                    stack2.push(*next);
                }
            }
            potential += best_weight;
        }
        if best.is_some_and(|best| best >= potential) || !used2.contains(&end) {
            continue;
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
