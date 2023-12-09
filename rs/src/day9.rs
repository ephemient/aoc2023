fn predict(nums: &[i32]) -> i64 {
    nums.iter()
        .enumerate()
        .fold((1, 0), |(c, s), (i, x)| {
            (c * (nums.len() - i) / (i + 1), c as i64 * *x as i64 - s)
        })
        .1
}

pub fn part1(data: &str) -> i64 {
    data.lines()
        .map(|line| {
            predict(
                &line
                    .split_whitespace()
                    .filter_map(|s| s.parse::<i32>().ok())
                    .collect::<Vec<_>>(),
            )
        })
        .sum()
}

pub fn part2(data: &str) -> i64 {
    data.lines()
        .map(|line| {
            predict(
                &line
                    .split_whitespace()
                    .rev()
                    .filter_map(|s| s.parse::<i32>().ok())
                    .collect::<Vec<_>>(),
            )
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        0 3 6 9 12 15
        1 3 6 10 15 21
        10 13 16 21 30 45
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(114, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(2, part2(EXAMPLE));
    }
}
