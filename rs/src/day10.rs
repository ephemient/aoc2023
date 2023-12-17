use static_init::dynamic;
use std::collections::BTreeMap;
use std::iter;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Direction {
    U,
    L,
    D,
    R,
}

#[dynamic]
static LUT: BTreeMap<(Direction, char), Direction> = [
    ((Direction::U, '|'), Direction::U),
    ((Direction::U, '7'), Direction::L),
    ((Direction::U, 'F'), Direction::R),
    ((Direction::L, '-'), Direction::L),
    ((Direction::L, 'F'), Direction::D),
    ((Direction::L, 'L'), Direction::U),
    ((Direction::D, '|'), Direction::D),
    ((Direction::D, 'L'), Direction::R),
    ((Direction::D, 'J'), Direction::L),
    ((Direction::R, '-'), Direction::R),
    ((Direction::R, 'J'), Direction::U),
    ((Direction::R, '7'), Direction::D),
]
.into();

fn step((y, x): (usize, usize), dir: Direction) -> Option<(usize, usize)> {
    Some(match dir {
        Direction::U => (y.checked_sub(1)?, x),
        Direction::L => (y, x.checked_sub(1)?),
        Direction::D => (y.checked_add(1)?, x),
        Direction::R => (y, x.checked_add(1)?),
    })
}

fn part1_helper(maze: &[&str]) -> Option<Vec<(usize, usize)>> {
    for (y, line) in maze.iter().enumerate() {
        for (x, char) in line.char_indices() {
            if char != 'S' {
                continue;
            }
            let start_pos = (y, x);
            for mut dir in [Direction::U, Direction::L, Direction::D, Direction::R] {
                let mut pos = start_pos;
                let mut path = iter::from_fn(|| {
                    pos = step(pos, dir)?;
                    dir =
                        *LUT.get(&(dir, maze[pos.0..].iter().next()?[pos.1..].chars().next()?))?;
                    Some(pos)
                })
                .collect::<Vec<_>>();
                if pos == start_pos && !path.is_empty() {
                    path.push(pos);
                    return Some(path);
                }
            }
        }
    }
    None
}

pub fn part1(data: &str) -> Option<usize> {
    Some(part1_helper(&data.lines().collect::<Vec<_>>())?.len() / 2)
}

pub fn part2(data: &str) -> Option<usize> {
    let path = part1_helper(&data.lines().collect::<Vec<_>>())?;
    let (n, m) = path
        .iter()
        .zip(path[1..].iter().chain(path.iter()))
        .map(|((y0, x0), (y1, x1))| (x0 * y1, x1 * y0))
        .fold((0, 0), |(a, b), (c, d)| (a + c, b + d));
    Some((2 + n.max(m) - n.min(m) - path.len()) / 2)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        -L|F7
        7S-7|
        L|7||
        -L-J|
        L|-JF
    "};
    static EXAMPLE_2: &str = indoc! {"
        7-F7-
        .FJ|7
        SJLL7
        |F--J
        LJ.LJ
    "};
    static EXAMPLE_3: &str = indoc! {"
        ...........
        .S-------7.
        .|F-----7|.
        .||.....||.
        .||.....||.
        .|L-7.F-J|.
        .|..|.|..|.
        .L--J.L--J.
        ...........
    "};
    static EXAMPLE_4: &str = indoc! {"
        ..........
        .S------7.
        .|F----7|.
        .||....||.
        .||....||.
        .|L-7F-J|.
        .|..||..|.
        .L--JL--J.
        ..........
    "};
    static EXAMPLE_5: &str = indoc! {"
        .F----7F7F7F7F-7....
        .|F--7||||||||FJ....
        .||.FJ||||||||L7....
        FJL7L7LJLJ||LJ.L-7..
        L--J.L7...LJS7F-7L7.
        ....F-J..F7FJ|L7L7L7
        ....L7.F7||L7|.L7L7|
        .....|FJLJ|FJ|F7|.LJ
        ....FJL-7.||.||||...
        ....L---J.LJ.LJLJ...
    "};
    static EXAMPLE_6: &str = indoc! {"
        FF7FSF7F7F7F7F7F---7
        L|LJ||||||||||||F--J
        FL-7LJLJ||||||LJL-77
        F--JF--7||LJLJ7F7FJ-
        L---JF-JLJ.||-FJLJJ7
        |F|F-JF---7F7-L7L|7|
        |FFJF7L7F-JF7|JL---7
        7-L-JL7||F7|L7F-7F7|
        L.L7LFJ|||||FJL7||LJ
        L7JLJL-JLJLJL--JLJ.L
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(4), part1(EXAMPLE_1));
        assert_eq!(Some(8), part1(EXAMPLE_2));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(4), part2(EXAMPLE_3));
        assert_eq!(Some(4), part2(EXAMPLE_4));
        assert_eq!(Some(8), part2(EXAMPLE_5));
        assert_eq!(Some(10), part2(EXAMPLE_6));
    }
}
