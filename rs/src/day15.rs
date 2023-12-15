use std::collections::HashMap;

fn hash(data: &str) -> u8 {
    data.bytes()
        .fold(0, |acc, b| 17u8.wrapping_mul(acc.wrapping_add(b)))
}

pub fn part1(data: &str) -> u32 {
    data.lines()
        .flat_map(|line| line.split(','))
        .map(|step| u32::from(hash(step)))
        .sum()
}

pub fn part2(data: &str) -> Option<usize> {
    let mut lenses = HashMap::<&str, (usize, usize)>::new();
    for (i, step) in data.lines().flat_map(|line| line.split(',')).enumerate() {
        if let Some(key) = step.strip_suffix('-') {
            lenses.remove(key);
        } else {
            let (key, value) = step.split_once('=')?;
            let value = value.parse::<usize>().ok()?;
            lenses
                .entry(key)
                .and_modify(|(entry, _)| {
                    *entry = value;
                })
                .or_insert((value, i));
        }
    }
    let mut boxes: [Vec<(usize, usize)>; 256] = std::array::from_fn(|_| vec![]);
    for (key, value) in lenses.into_iter() {
        boxes[usize::from(hash(key))].push(value);
    }
    Some(
        boxes
            .into_iter()
            .enumerate()
            .map(|(i, mut boxes)| {
                boxes.sort_by_key(|(_, c)| *c);
                (i + 1)
                    * boxes
                        .into_iter()
                        .enumerate()
                        .map(|(j, (value, _))| (j + 1) * value)
                        .sum::<usize>()
            })
            .sum::<usize>(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(1320, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(145), part2(EXAMPLE));
    }
}
