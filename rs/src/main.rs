use aoc2023::day1;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::io;
use std::path::Path;

fn get_day_input(day: u8) -> Result<String, io::Error> {
    let datadir = env::var("AOC2023_DATADIR")
        .ok()
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| ".".to_string());
    fs::read_to_string(Path::new(&datadir).join(format!("day{}.txt", day)))
}

fn main() -> io::Result<()> {
    let args = env::args().skip(1).collect::<HashSet<_>>();

    if args.is_empty() || args.contains("1") {
        let data = get_day_input(1)?;
        println!("Day 1");
        println!("{:?}", day1::part1(&data));
        println!("{:?}", day1::part2(&data));
        println!();
    }

    Ok(())
}
