fn predict(nums: &[i32]) -> i32 {
    if nums.iter().any(|&x| x != 0) {
        nums.last().unwrap()
            + predict(
                &nums
                    .iter()
                    .zip(nums.iter().skip(1))
                    .map(|(x, y)| y - x)
                    .collect::<Vec<_>>(),
            )
    } else {
        0
    }
}

pub fn part1(data: &str) -> i32 {
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

pub fn part2(data: &str) -> i32 {
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
