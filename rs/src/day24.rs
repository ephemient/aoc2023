use std::cmp::Ordering;
use std::ops::RangeInclusive;

fn part1_internal(data: &str, lo: f64, hi: f64) -> Option<usize> {
    let mut lines = Vec::<(f64, f64, RangeInclusive<f64>)>::new();
    data.lines()
        .map(|line| -> Option<usize> {
            let (pos, vel) = line.split_once('@')?;
            let mut iter = pos.splitn(3, ',');
            let (x0, y0, _) = (
                iter.next()?.trim().parse::<f64>().ok()?,
                iter.next()?.trim().parse::<f64>().ok()?,
                iter.next()?.trim().parse::<f64>().ok()?,
            );
            let mut iter = vel.splitn(3, ',');
            let (vx0, vy0, _) = (
                iter.next()?.trim().parse::<f64>().ok()?,
                iter.next()?.trim().parse::<f64>().ok()?,
                iter.next()?.trim().parse::<f64>().ok()?,
            );
            let m0 = vy0 / vx0;
            let b0 = y0 - m0 * x0;
            let range0 = match vx0.partial_cmp(&0.0)? {
                Ordering::Less => -f64::INFINITY..=x0,
                Ordering::Equal => return None,
                Ordering::Greater => x0..=f64::INFINITY,
            };
            let n = lines
                .iter()
                .filter(|(m1, b1, range1)| {
                    let x = (b0 - b1) / (m1 - m0);
                    m0 != *m1
                        && (lo..=hi).contains(&x)
                        && (lo..=hi).contains(&(m0 * x + b0))
                        && range0.contains(&x)
                        && range1.contains(&x)
                })
                .count();
            lines.push((m0, b0, range0));
            Some(n)
        })
        .try_fold(0, |acc, n| Some(acc + n?))
}

pub fn part1(data: &str) -> Option<usize> {
    part1_internal(data, 2e14, 4e14)
}

pub fn part2(_data: &str) -> Option<usize> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        19, 13, 30 @ -2,  1, -2
        18, 19, 22 @ -1, -1, -2
        20, 25, 34 @ -2, -2, -4
        12, 31, 28 @ -1, -2, -1
        20, 19, 15 @  1, -5, -3
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(2), part1_internal(EXAMPLE, 7.0, 27.0));
    }

    #[test]
    #[ignore]
    fn part2_examples() {
        assert_eq!(Some(47), part2(EXAMPLE));
    }
}
