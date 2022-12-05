use aoc22::read_file;
use std::error::Error;

type Input<'a> = &'a str;

fn decode_priority(c: char) -> u32 {
    match c {
        'a'..='z' => c as u32 - 'a' as u32,
        'A'..='Z' => c as u32 - 'A' as u32 + 26,
        _ => 0,
    }
}

fn part1(input: &[Input]) -> Result<u32, Box<dyn Error>> {
    Ok(input
        .iter()
        .map(|pack| {
            let mut mask = 0u64;
            for (i, c) in pack.chars().enumerate() {
                let pri = decode_priority(c);
                if i < pack.len() / 2 {
                    mask |= 1 << pri;
                } else if (mask & (1 << pri)) != 0 {
                    return pri + 1;
                }
            }
            0
        })
        .sum())
}

fn part2(input: &[Input]) -> Result<u32, Box<dyn Error>> {
    Ok(input
        .chunks(3)
        .map(|group| {
            let mut mask = u64::MAX;
            for &pack in group {
                let mut pack_val = 0;
                for c in pack.chars() {
                    pack_val |= 1 << decode_priority(c);
                }
                mask &= pack_val;
            }
            mask.trailing_zeros() + 1
        })
        .sum())
}

fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day3")?;
    let contents: Vec<_> = binding.iter().map(|ln| &**ln).collect();
    println!("Prob1: {}", part1(&contents)?);
    println!("Prob2: {}", part2(&contents)?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{part1, part2};
    const TEST_DATA: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";
    #[test]
    fn test1() {
        let contents: Vec<_> = TEST_DATA.lines().collect();
        assert_eq!(157, part1(&contents).unwrap());
    }
    #[test]
    fn test2() {
        let contents: Vec<_> = TEST_DATA.lines().collect();
        assert_eq!(70, part2(&contents).unwrap());
    }
}
