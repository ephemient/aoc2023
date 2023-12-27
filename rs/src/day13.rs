fn find_reflection<T: Eq, F: Fn(&[T], &[T]) -> bool>(data: &[T], eq: F) -> usize {
    (1..data.len())
        .find(|&i| eq(&data[..i], &data[i..]))
        .unwrap_or_default()
}

fn solve<F: Fn(&[&[u8]], &[&[u8]]) -> bool>(lines: &[&[u8]], eq: F) -> usize {
    let width = lines
        .iter()
        .map(|line| line.len())
        .max()
        .unwrap_or_default();
    let transpose = (0..width)
        .map(|i| lines.iter().map(|line| line[i]).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let transpose = transpose.iter().map(|line| &line[..]).collect::<Vec<_>>();

    100 * find_reflection(lines, &eq) + find_reflection(&transpose, &eq)
}

pub fn part1(data: &str) -> usize {
    data.split("\n\n")
        .map(|group| {
            solve(
                &group
                    .lines()
                    .map(|line| line.as_bytes())
                    .collect::<Vec<_>>(),
                |x, y| x.iter().rev().zip(y.iter()).all(|(a, b)| a == b),
            )
        })
        .sum()
}

pub fn part2(data: &str) -> usize {
    data.split("\n\n")
        .map(|group| {
            solve(
                &group
                    .lines()
                    .map(|line| line.as_bytes())
                    .collect::<Vec<_>>(),
                |x, y| {
                    let mut iter = x
                        .iter()
                        .rev()
                        .zip(y.iter())
                        .flat_map(|(a, b)| a.iter().zip(b.iter()).filter(|(c, d)| c != d));
                    iter.next().is_some() && iter.next().is_none()
                },
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
        #.##..##.
        ..#.##.#.
        ##......#
        ##......#
        ..#.##.#.
        ..##..##.
        #.#.##.#.

        #...##..#
        #....#..#
        ..##..###
        #####.##.
        #####.##.
        ..##..###
        #....#..#
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(405, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(400, part2(EXAMPLE));
    }
}
