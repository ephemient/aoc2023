use std::collections::BTreeMap;

fn parse(data: &str) -> BTreeMap<(usize, usize), Vec<u32>> {
    let lines = data.split_terminator('\n').collect::<Vec<_>>();
    let mut map = BTreeMap::<(usize, usize), Vec<u32>>::new();
    for (y, &line) in lines.iter().enumerate() {
        let mut x = 0;
        let mut s = line;
        while let Some(off) = s.find(|c: char| c.is_ascii_digit()) {
            let t = &s[off..];
            let (t, u) = t.split_at(t.find(|c: char| !c.is_ascii_digit()).unwrap_or(t.len()));
            let number = t.parse::<u32>().expect("char::is_ascii_digit span");
            for (y, &line) in lines
                .iter()
                .enumerate()
                .take(y.saturating_add(2))
                .skip(y.saturating_sub(1))
            {
                let x0 = (x + off).saturating_sub(1);
                let x1 = (x + off + t.len()).saturating_add(1).min(line.len());
                for (off, c) in line[x0..x1].char_indices() {
                    if !(c == '.' || c.is_ascii_whitespace() || c.is_ascii_digit()) {
                        map.entry((x0 + off, y))
                            .and_modify(|v| v.push(number))
                            .or_insert_with(|| vec![number]);
                    }
                }
            }
            x += off + t.len();
            s = u;
        }
    }
    map
}

pub fn part1(data: &str) -> u32 {
    parse(data).values().flatten().sum()
}

pub fn part2(data: &str) -> u32 {
    parse(data)
        .values()
        .filter(|v| v.len() == 2)
        .map(|v| v.iter().product::<u32>())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        467..114..
        ...*......
        ..35..633.
        ......#...
        617*......
        .....+.58.
        ..592.....
        ......755.
        ...$.*....
        .664.598..
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(4361, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(467835, part2(EXAMPLE));
    }
}
