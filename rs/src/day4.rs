use std::collections::HashSet;

fn parse(data: &str) -> Vec<usize> {
    data.lines()
        .filter_map(|line| {
            let (left, right) = line[line.find(':')? + 1..].split_once('|')?;
            Some(
                left.split_whitespace()
                    .collect::<HashSet<_>>()
                    .intersection(&right.split_whitespace().collect())
                    .count(),
            )
        })
        .collect()
}

pub fn part1(data: &str) -> usize {
    parse(data).into_iter().map(|card| 1 << card >> 1).sum()
}

pub fn part2(data: &str) -> usize {
    let cards = parse(data);
    let mut counts = vec![1; cards.len()];
    for (i, card) in cards.into_iter().enumerate() {
        for j in 1..=card {
            if i + j >= counts.len() {
                break;
            }
            counts[i + j] += counts[i];
        }
    }
    counts.into_iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
        Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
        Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
        Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
        Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
        Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(13, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(30, part2(EXAMPLE));
    }
}
