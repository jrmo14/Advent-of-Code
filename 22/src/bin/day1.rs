use aoc22::read_file;
use std::error::Error;

fn part1(input: &[i32]) -> Result<i32, Box<dyn Error>> {
    let mut max_elf = 0;
    let mut cur_elf = 0;
    for &ln in input {
        if ln < 0 {
            if cur_elf > max_elf {
                max_elf = cur_elf;
            }
            cur_elf = 0;
        } else {
            cur_elf += ln;
        }
    }
    Ok(max_elf)
}

fn part2(input: &[i32]) -> Result<i32, Box<dyn Error>> {
    let mut elfs = Vec::new();
    let mut cur_elf = 0;
    for &ln in input {
        if ln < 0 {
            elfs.push(cur_elf);
            cur_elf = 0;
        } else {
            cur_elf += ln;
        }
    }
    elfs.sort();
    Ok(elfs.iter().rev().take(3).sum())
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents: Vec<_> = read_file("res/day1")?
        .iter()
        .map(|ln| ln.parse::<i32>().unwrap_or(-1))
        .collect();
    println!("Prob1: {}", part1(&contents)?);
    println!("Prob2: {}", part2(&contents)?);
    Ok(())
}
