use itertools::Itertools;
use rayon::iter::{ParallelBridge, ParallelIterator};

fn solve<const N: usize>(line: &str) -> Option<usize> {
    let (lhs, rhs) = line.split_once(' ')?;
    let rhs = rhs
        .split(',')
        .map(|x| x.parse().ok())
        .collect::<Option<Vec<_>>>()?;
    let string = Itertools::intersperse([&lhs; N].into_iter(), &"?")
        .flat_map(|s| s.chars())
        .collect::<Vec<_>>();
    let runs = [&rhs; N].into_iter().flatten().copied().collect::<Vec<_>>();
    let last_run = runs.last()?;
    let counts = runs.iter().rev().skip(1).fold(
        (0..string.len())
            .map(|i| {
                if i + last_run > string.len()
                    || i != 0 && string[i - 1] == '#'
                    || string[i..i + last_run].iter().any(|&c| c == '.')
                    || string[i + last_run..].iter().any(|&c| c == '#')
                {
                    0
                } else {
                    1
                }
            })
            .collect::<Vec<_>>(),
        |counts, run| {
            (0..string.len())
                .map(|i| {
                    if i + run >= string.len()
                        || i != 0 && string[i - 1] == '#'
                        || string[i..i + run].iter().any(|&c| c == '.')
                        || string[i + run] == '#'
                    {
                        0
                    } else {
                        counts[i + run + 1..]
                            .iter()
                            .zip(
                                string[i + run + 1..]
                                    .iter()
                                    .take_while(|&&c| c != '#')
                                    .chain([&'#']),
                            )
                            .map(|(&count, _)| count)
                            .sum()
                    }
                })
                .collect()
        },
    );
    Some(
        counts
            .into_iter()
            .zip(string.into_iter().take_while(|&c| c != '#').chain(['#']))
            .map(|(count, _)| count)
            .sum(),
    )
}

pub fn part1(data: &str) -> usize {
    data.lines().filter_map(solve::<1>).sum()
}

pub fn part2(data: &str) -> usize {
    data.lines().par_bridge().filter_map(solve::<5>).sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        ???.### 1,1,3
        .??..??...?##. 1,1,3
        ?#?#?#?#?#?#?#? 1,3,1,6
        ????.#...#... 4,1,1
        ????.######..#####. 1,6,5
        ?###???????? 3,2,1
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(21, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(525152, part2(EXAMPLE));
    }
}
