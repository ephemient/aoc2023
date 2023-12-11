fn solve(data: &str, n: usize) -> usize {
    let lines = data.lines().collect::<Vec<_>>();
    solve1(
        &lines
            .iter()
            .map(|line| line.chars().filter(|&c| c == '#').count())
            .collect::<Vec<_>>(),
        n,
    ) + solve1(
        &(0..lines
            .iter()
            .map(|line| line.len())
            .max()
            .unwrap_or_default())
            .map(|x| {
                lines
                    .iter()
                    .filter(|line| line[x..].starts_with('#'))
                    .count()
            })
            .collect::<Vec<_>>(),
        n,
    )
}

fn solve1(data: &[usize], n: usize) -> usize {
    data.iter()
        .enumerate()
        .map(|(i, &a)| {
            if a == 0 {
                0
            } else {
                data[i + 1..]
                    .iter()
                    .scan(0, move |m, &b| {
                        *m += if b == 0 { n } else { 1 };
                        Some(*m * a * b)
                    })
                    .sum()
            }
        })
        .sum()
}

pub fn part1(data: &str) -> usize {
    solve(data, 2)
}

pub fn part2(data: &str) -> usize {
    solve(data, 1000000)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        ...#......
        .......#..
        #.........
        ..........
        ......#...
        .#........
        .........#
        ..........
        .......#..
        #...#.....
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(374, solve(EXAMPLE, 2));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(1030, solve(EXAMPLE, 10));
        assert_eq!(8410, solve(EXAMPLE, 100));
    }
}
