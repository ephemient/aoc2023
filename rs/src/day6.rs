use std::num::ParseIntError;

fn win_count(time: u64, distance: u64) -> u64 {
    let b = time as f64 / 2.0;
    let d = (b * b - distance as f64).sqrt();
    ((b + d - 1.0).ceil() - (b - d + 1.0).floor() + 1.0) as u64
}

pub fn part1(data: &str) -> u64 {
    let mut lines = data.lines();
    lines
        .next()
        .unwrap_or("")
        .split(|c: char| !c.is_ascii_digit())
        .filter_map(|s| s.parse().ok())
        .zip(
            lines
                .next()
                .unwrap_or("")
                .split(|c: char| !c.is_ascii_digit())
                .filter_map(|s| s.parse().ok()),
        )
        .map(|(time, distance)| win_count(time, distance))
        .product()
}

pub fn part2(data: &str) -> Result<u64, ParseIntError> {
    let mut lines = data.lines();
    let time = lines
        .next()
        .unwrap_or("")
        .chars()
        .filter(char::is_ascii_digit)
        .collect::<String>()
        .parse()?;
    let distance = lines
        .next()
        .unwrap_or("")
        .chars()
        .filter(char::is_ascii_digit)
        .collect::<String>()
        .parse()?;
    Ok(win_count(time, distance))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        Time:      7  15   30
        Distance:  9  40  200
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(288, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Ok(71503), part2(EXAMPLE));
    }
}
