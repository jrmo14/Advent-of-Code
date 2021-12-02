use aoc21::read_file;
use itertools::Itertools;
use std::error::Error;

fn part1(input: &Vec<String>) -> Result<i32, Box<dyn Error>> {
    todo!()
}

fn part2(input: &Vec<String>) -> Result<i32, Box<dyn Error>> {
    todo!()
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents = read_file()?;
    println!("Prob1: {}", part1(&contents)?);
    println!("Prob2: {}", part2(&contents)?);
    Ok(())
}
