fn solve(data: &str, n: u64) -> u64 {
    let lines = data.lines().collect::<Vec<_>>();
    let rows = lines
        .iter()
        .enumerate()
        .filter_map(|(y, line)| {
            if line.chars().any(|c| c == '#') {
                None
            } else {
                Some(y)
            }
        })
        .collect::<Vec<_>>();
    let cols = (0..lines
        .iter()
        .map(|line| line.len())
        .max()
        .unwrap_or_default())
        .filter(|&x| lines.iter().all(|line| !line[x..].starts_with('#')))
        .collect::<Vec<_>>();
    let galaxies = lines
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.char_indices()
                .filter_map(move |(x, c)| if c == '#' { Some((y, x)) } else { None })
        })
        .collect::<Vec<_>>();
    galaxies
        .iter()
        .enumerate()
        .flat_map(|(i, (y0, x0))| {
            let rows = &rows;
            let cols = &cols;
            galaxies[i + 1..].iter().map(move |(y1, x1)| {
                let (x0, x1) = (x0.min(x1), x0.max(x1));
                (y1 - y0 + x1 - x0) as u64
                    + (n - 1)
                        * (rows.binary_search(y1).unwrap_err()
                            - rows.binary_search(y0).unwrap_err()
                            + cols.binary_search(x1).unwrap_err()
                            - cols.binary_search(x0).unwrap_err()) as u64
            })
        })
        .sum()
}

pub fn part1(data: &str) -> u64 {
    solve(data, 2)
}

pub fn part2(data: &str) -> u64 {
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
