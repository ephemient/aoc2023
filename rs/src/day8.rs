use num_integer::lcm;
use rayon::iter::{ParallelBridge, ParallelIterator};
use std::{collections::BTreeMap, iter};

struct Network<'a> {
    instructions: Vec<bool>,
    table: BTreeMap<&'a str, (&'a str, &'a str)>,
}

impl<'a> Network<'a> {
    fn step(&self, start: &'a str) -> Option<&'a str> {
        self.instructions
            .iter()
            .try_fold(start, |node, &instruction| {
                let (left, right) = self.table.get(node)?;
                Some(*if instruction { left } else { right })
            })
    }
}

fn parse(data: &str) -> Option<Network> {
    let mut iter = data.lines();
    let instructions = iter
        .next()?
        .chars()
        .map(|c| match c {
            'L' => Some(true),
            'R' => Some(false),
            _ => None,
        })
        .collect::<Option<_>>()?;
    let table = iter
        .filter(|s| !s.is_empty())
        .map(|line| {
            let (from, to) = line.split_once(" = ")?;
            let (left, right) = to.split_once(", ")?;
            Some((from, (left.strip_prefix('(')?, right.strip_suffix(')')?)))
        })
        .collect::<Option<_>>()?;
    Some(Network {
        instructions,
        table,
    })
}

pub fn part1(data: &str) -> Option<usize> {
    let network = parse(data)?;
    Some(
        network.instructions.len()
            * iter::successors(Some("AAA"), |&node| network.step(node))
                .position(|node| node == "ZZZ")?,
    )
}

pub fn part2(data: &str) -> Option<usize> {
    let network = parse(data)?;
    Some(
        network.instructions.len()
            * network
                .table
                .keys()
                .filter(|node| node.ends_with('A'))
                .par_bridge()
                .map(|&start| {
                    let (i, end) = iter::successors(Some(start), |&node| network.step(node))
                        .enumerate()
                        .find(|(_, node)| node.ends_with('Z'))?;
                    if network.step(start) == network.step(end) {
                        Some(i)
                    } else {
                        None
                    }
                })
                .try_reduce(|| 1, |x, y| Some(lcm(x, y)))?,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        RL
        
        AAA = (BBB, CCC)
        BBB = (DDD, EEE)
        CCC = (ZZZ, GGG)
        DDD = (DDD, DDD)
        EEE = (EEE, EEE)
        GGG = (GGG, GGG)
        ZZZ = (ZZZ, ZZZ)
    "};
    static EXAMPLE_2: &str = indoc! {"
        LLR
        
        AAA = (BBB, BBB)
        BBB = (AAA, ZZZ)
        ZZZ = (ZZZ, ZZZ)
    "};
    static EXAMPLE_3: &str = indoc! {"
        LR
        
        11A = (11B, XXX)
        11B = (XXX, 11Z)
        11Z = (11B, XXX)
        22A = (22B, XXX)
        22B = (22C, 22C)
        22C = (22Z, 22Z)
        22Z = (22B, 22B)
        XXX = (XXX, XXX)
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(2), part1(EXAMPLE_1));
        assert_eq!(Some(6), part1(EXAMPLE_2));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(6), part2(EXAMPLE_3));
    }
}
