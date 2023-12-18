enum Direction {
    R,
    D,
    L,
    U,
}

fn solve<I: IntoIterator<Item = (Direction, u32)>>(data: I) -> u64 {
    let (mut x, mut y, mut a, mut l) = (0, 0, 0, 0);
    for (d, n) in data {
        match d {
            Direction::R => {
                x += i64::from(n);
                a += y * i64::from(n);
            }
            Direction::D => {
                y += i64::from(n);
            }
            Direction::L => {
                x -= i64::from(n);
                a -= y * i64::from(n);
            }
            Direction::U => {
                y -= i64::from(n);
            }
        }
        l += u64::from(n);
    }
    debug_assert_eq!((x, y), (0, 0));
    a.unsigned_abs() + l / 2 + 1
}

pub fn part1(data: &str) -> u64 {
    solve(data.lines().filter_map(|line| {
        let mut iter = line.split(' ');
        let d = iter.next()?;
        let d = if d == "R" {
            Direction::R
        } else if d == "D" {
            Direction::D
        } else if d == "L" {
            Direction::L
        } else if d == "U" {
            Direction::U
        } else {
            return None;
        };
        Some((d, iter.next()?.parse::<u32>().ok()?))
    }))
}

pub fn part2(data: &str) -> u64 {
    solve(data.lines().filter_map(|line| {
        let line = line
            .split(' ')
            .next_back()?
            .strip_prefix("(#")?
            .strip_suffix(')')?;
        let d = match line.bytes().last()? {
            b'0' => Direction::R,
            b'1' => Direction::D,
            b'2' => Direction::L,
            b'3' => Direction::U,
            _ => {
                return None;
            }
        };
        Some((d, u32::from_str_radix(&line[..line.len() - 1], 16).ok()?))
    }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        R 6 (#70c710)
        D 5 (#0dc571)
        L 2 (#5713f0)
        D 2 (#d2c081)
        R 2 (#59c680)
        D 2 (#411b91)
        L 5 (#8ceee2)
        U 2 (#caa173)
        L 1 (#1b58a2)
        U 2 (#caa171)
        R 2 (#7807d2)
        U 3 (#a77fa3)
        L 2 (#015232)
        U 2 (#7a21e3)
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(62, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(952408144115, part2(EXAMPLE));
    }
}
