use std::collections::BTreeSet;
use std::iter;

#[allow(clippy::type_complexity)]
fn parse(data: &str) -> Option<(Vec<&[u8]>, (usize, usize))> {
    let grid = data.lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    let start = grid.iter().enumerate().find_map(|(y, row)| {
        row.iter()
            .enumerate()
            .find_map(move |(x, b)| if *b == b'S' { Some((y, x)) } else { None })
    })?;
    Some((grid, start))
}

fn dec((m, r): (isize, usize), n: usize) -> (isize, usize) {
    r.checked_sub(1).map_or((m - 1, n - 1), |r| (m, r))
}

fn inc((m, r): (isize, usize), n: usize) -> (isize, usize) {
    r.checked_add(1)
        .filter(|r| *r < n)
        .map_or((m + 1, 0), |r| (m, r))
}

fn bfs<'a>(grid: &'a [&'a [u8]], start: (usize, usize)) -> impl Iterator<Item = usize> + 'a {
    let (mut d, mut even, mut odd) = (0, 0, 0);
    let mut v = BTreeSet::<((isize, usize), (isize, usize))>::new();
    let mut q = BTreeSet::from([((0, start.0), (0, start.1))]);
    iter::from_fn(move || {
        let n = if d & 1 == 0 { &mut even } else { &mut odd };
        *n += q.len();
        d += 1;
        v.extend(q.iter());
        q = q
            .iter()
            .flat_map(|p| {
                let h = grid.len();
                let w = grid[p.0 .1].len();
                [
                    (dec(p.0, h), p.1),
                    (p.0, dec(p.1, w)),
                    (p.0, inc(p.1, w)),
                    (inc(p.0, h), p.1),
                ]
            })
            .filter(|p| !v.contains(p) && grid[p.0 .1][p.1 .1] != b'#')
            .collect();
        Some(*n)
    })
}

fn part1_n(data: &str, n: usize) -> Option<usize> {
    let (grid, start) = parse(data)?;
    let mut iter = bfs(&grid, start);
    iter.nth(n)
}

pub fn part1(data: &str) -> Option<usize> {
    part1_n(data, 64)
}

fn part2_n(data: &str, n: usize) -> Option<usize> {
    let (grid, start) = parse(data)?;
    let m = grid.len();
    let (q, r) = (n / m, n % m);
    let mut iter = bfs(&grid, start).skip(r).step_by(m);
    let (a, b, c, d) = (iter.next()?, iter.next()?, iter.next()?, iter.next()?);
    if a + 3 * (c - b) == d {
        Some(a + (b - a) * q + (a + c - 2) * (q * q.saturating_sub(1) / 2))
    } else {
        None
    }
}

pub fn part2(data: &str) -> Option<usize> {
    part2_n(data, 26501365)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        ...........
        .....###.#.
        .###.##..#.
        ..#.#...#..
        ....#.#....
        .##..S####.
        .##..#...#.
        .......##..
        .##.#.####.
        .##..##.##.
        ...........
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(2), part1_n(EXAMPLE, 1));
        assert_eq!(Some(4), part1_n(EXAMPLE, 2));
        assert_eq!(Some(6), part1_n(EXAMPLE, 3));
        assert_eq!(Some(16), part1_n(EXAMPLE, 6));
    }

    #[test]
    #[ignore = "non-general solution"]
    fn part2_examples() {
        assert_eq!(Some(16), part2_n(EXAMPLE, 6));
        assert_eq!(Some(50), part2_n(EXAMPLE, 10));
        assert_eq!(Some(1594), part2_n(EXAMPLE, 50));
        assert_eq!(Some(6536), part2_n(EXAMPLE, 100));
        assert_eq!(Some(167004), part2_n(EXAMPLE, 500));
        assert_eq!(Some(668697), part2_n(EXAMPLE, 1000));
        assert_eq!(Some(16733044), part2_n(EXAMPLE, 5000));
    }
}
