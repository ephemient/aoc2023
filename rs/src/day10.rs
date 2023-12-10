use static_init::dynamic;
use std::collections::BTreeMap;
use std::ops::Neg;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Direction {
    U,
    L,
    D,
    R,
}

impl Neg for Direction {
    type Output = Direction;

    fn neg(self) -> Self::Output {
        match self {
            Direction::U => Direction::D,
            Direction::L => Direction::R,
            Direction::D => Direction::U,
            Direction::R => Direction::L,
        }
    }
}

#[dynamic]
static SYMBOLS: BTreeMap<char, [Direction; 2]> = [
    ('|', [Direction::U, Direction::D]),
    ('-', [Direction::L, Direction::R]),
    ('L', [Direction::U, Direction::R]),
    ('J', [Direction::U, Direction::L]),
    ('7', [Direction::L, Direction::D]),
    ('F', [Direction::D, Direction::R]),
]
.into_iter()
.collect();

type Position = (usize, usize);

fn step(dir: Direction, (y, x): Position) -> Option<Position> {
    Some(match dir {
        Direction::U => (y.checked_sub(1)?, x),
        Direction::L => (y, x.checked_sub(1)?),
        Direction::D => (y.checked_add(1)?, x),
        Direction::R => (y, x.checked_add(1)?),
    })
}

fn part1_helper(maze: &[&str]) -> Option<(Position, [Direction; 2], Vec<Position>)> {
    for (y, line) in maze.iter().enumerate() {
        for (x, char) in line.char_indices() {
            if char != 'S' {
                continue;
            }
            let start_pos = (y, x);
            'dir: for start_dir in [Direction::U, Direction::L, Direction::D, Direction::R] {
                let Some(mut pos) = step(start_dir, start_pos) else {
                    continue 'dir;
                };
                let mut last_dir = -start_dir;
                let mut path = vec![start_pos];
                while pos != start_pos {
                    let Some(dirs) = maze[pos.0..]
                        .iter()
                        .next()
                        .and_then(|line| line[pos.1..].chars().next())
                        .and_then(|char| SYMBOLS.get(&char))
                    else {
                        continue 'dir;
                    };
                    if !dirs.iter().any(|&dir| dir == last_dir) {
                        continue 'dir;
                    }
                    let Some(&next_dir) = dirs.iter().find(|&&dir| dir != last_dir) else {
                        continue 'dir;
                    };
                    let Some(next_pos) = step(next_dir, pos) else {
                        continue 'dir;
                    };
                    path.push(pos);
                    pos = next_pos;
                    last_dir = -next_dir;
                }
                return Some((start_pos, [start_dir, last_dir], path));
            }
        }
    }
    None
}

pub fn part1(data: &str) -> Option<usize> {
    Some(part1_helper(&data.lines().collect::<Vec<_>>())?.2.len() / 2)
}

pub fn part2(data: &str) -> Option<usize> {
    let maze = data.lines().collect::<Vec<_>>();
    let (start_pos, start_dirs, mut path) = part1_helper(&maze)?;
    path.sort();
    Some(
        maze.iter()
            .enumerate()
            .flat_map(|(y, line)| line.char_indices().map(move |(x, char)| ((y, x), char)))
            .scan(
                (false, false, path.into_iter().peekable()),
                |(up, down, iter), (pos, char)| {
                    if Some(pos) == iter.peek().copied() {
                        iter.next()?;
                        for dir in if pos == start_pos {
                            &start_dirs
                        } else {
                            SYMBOLS.get(&char)?
                        } {
                            match dir {
                                Direction::U => *up = !*up,
                                Direction::D => *down = !*down,
                                _ => {}
                            }
                        }
                        Some(None)
                    } else if *up && *down {
                        Some(Some(()))
                    } else {
                        assert!(*up == *down);
                        Some(None)
                    }
                },
            )
            .flatten()
            .count(),
    )
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
