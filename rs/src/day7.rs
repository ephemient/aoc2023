use itertools::Itertools;
use std::cmp::Ordering;

fn rank(hand: &[Option<usize>]) -> u8 {
    let counts = hand.iter().filter_map(|x| *x).counts();
    let mut iter = counts.values().sorted().rev();
    let count0 = iter.next().copied().unwrap_or_default();
    let count1 = iter.next().copied().unwrap_or_default();
    let jokers = hand.iter().filter(|c| c.is_none()).count();
    if count0 + jokers >= 5 {
        6
    } else if count0 + jokers >= 4 {
        5
    } else if count0 + count1 + jokers >= 5 {
        4
    } else if count0 + jokers >= 3 {
        3
    } else if count0 + count1 + jokers >= 4 {
        2
    } else if count0 + jokers >= 2 {
        1
    } else {
        0
    }
}

fn solve(cards: &[char], data: &str) -> usize {
    data.lines()
        .filter_map(|line| {
            let (hand, bid) = line.split_once(' ')?;
            let hand = hand
                .chars()
                .map(|c| cards.iter().position(|&d| c == d))
                .collect::<Vec<_>>();
            let bid = bid.parse::<usize>().ok()?;
            Some((hand, bid))
        })
        .sorted_unstable_by(|(a, _), (b, _)| {
            a.iter()
                .zip(b.iter())
                .map(|(a, b)| a.cmp(b))
                .find(|&c| c != Ordering::Equal)
                .unwrap_or_else(|| a.len().cmp(&b.len()))
        })
        .sorted_by_cached_key(|(hand, _)| rank(hand))
        .enumerate()
        .map(|(i, (_, bid))| (i + 1) * bid)
        .sum()
}

pub fn part1(data: &str) -> usize {
    solve(&"23456789TJQKA".chars().collect::<Vec<_>>(), data)
}

pub fn part2(data: &str) -> usize {
    solve(&"23456789TQKA".chars().collect::<Vec<_>>(), data)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        32T3K 765
        T55J5 684
        KK677 28
        KTJJT 220
        QQQJA 483
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(6440, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(5905, part2(EXAMPLE));
    }
}
