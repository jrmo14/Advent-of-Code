use aoc22::{read_file, ProblemError};
use std::{collections::HashSet, error::Error};
type Input = str;
type Output = usize;

fn part1(input: &Input) -> Result<Output, Box<dyn Error>> {
    let input_bytes = input.as_bytes();
    for (i, window) in input_bytes.windows(4).enumerate() {
        let mut uniq = HashSet::new();
        for el in window {
            uniq.insert(el);
        }
        if uniq.len() == 4 {
            return Ok(i + 4);
        }
    }
    Err(Box::new(ProblemError {}))
}

fn part2(input: &Input) -> Result<Output, Box<dyn Error>> {
    let input_bytes = input.as_bytes();
    for (i, window) in input_bytes.windows(14).enumerate() {
        let mut uniq = HashSet::new();
        for el in window {
            uniq.insert(el);
        }
        if uniq.len() == 14 {
            return Ok(i + 14);
        }
    }
    Err(Box::new(ProblemError {}))
}

fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day6")?;

    println!("Part1: {}", part1(&binding[0])?);
    println!("Part2: {}", part2(&binding[0])?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{part1, part2};
    const TEST_DATA: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";

    #[test]
    fn test1() {
        assert_eq!(7, part1(&TEST_DATA).unwrap());
        assert_eq!(6, part1("nppdvjthqldpwncqszvftbrmjlhg").unwrap());
    }
}
