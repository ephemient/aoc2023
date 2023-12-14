use std::collections::{hash_map::Entry, HashMap};

fn parse(data: &str) -> (Vec<u8>, usize, usize) {
    let lines = data.lines().map(|line| line.as_bytes()).collect::<Vec<_>>();
    let width = lines
        .iter()
        .map(|line| line.len())
        .max()
        .unwrap_or_default();
    (
        (0..width)
            .flat_map(|x| {
                lines
                    .iter()
                    .map(move |line| if x < line.len() { line[x] } else { b'.' })
            })
            .collect(),
        width,
        lines.len(),
    )
}

fn tilt(data: &mut [u8], height: usize) {
    for col in data.chunks_mut(height) {
        for group in col.split_mut(|&c| c == b'#') {
            let n = group.iter().filter(|&&c| c == b'O').count();
            group[..n].fill(b'O');
            group[n..].fill(b'.');
        }
    }
}

fn rotate(src: &[u8], dst: &mut [u8], width: usize, height: usize) {
    for (i, c) in (0..height)
        .rev()
        .flat_map(|y| (0..width).map(move |x| src[x * height + y]))
        .enumerate()
    {
        dst[i] = c;
    }
}

fn spin(data: &mut [u8], temp: &mut [u8], width: usize, height: usize) {
    tilt(data, height);
    rotate(data, temp, width, height);
    tilt(temp, width);
    rotate(temp, data, height, width);
    tilt(data, height);
    rotate(data, temp, width, height);
    tilt(temp, width);
    rotate(temp, data, height, width);
}

fn load(data: &[u8], height: usize) -> usize {
    data.chunks(height)
        .flat_map(|col| {
            col.iter()
                .enumerate()
                .filter_map(|(y, &c)| if c == b'O' { Some(height - y) } else { None })
        })
        .sum()
}

pub fn part1(data: &str) -> usize {
    let (mut data, _, height) = parse(data);
    tilt(&mut data, height);
    load(&data, height)
}

const N: usize = 1000000000;

pub fn part2(data: &str) -> usize {
    let (mut data, width, height) = parse(data);
    let mut temp = vec![0; data.len()];
    let mut cache = [(data.clone(), 0)].into_iter().collect::<HashMap<_, _>>();
    for i in 1..=N {
        spin(&mut data, &mut temp, width, height);
        match cache.entry(data.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(i);
            }
            Entry::Occupied(entry) => {
                for _ in 0..(N - i) % (i - *entry.get()) {
                    spin(&mut data, &mut temp, width, height);
                }
                break;
            }
        }
    }
    load(&data, height)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        O....#....
        O.OO#....#
        .....##...
        OO.#O....O
        .O.....O#.
        O.#..O.#.#
        ..O..#O..O
        .......O..
        #....###..
        #OO..#....
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(136, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(64, part2(EXAMPLE));
    }
}
