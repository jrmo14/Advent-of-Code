use aoc22::read_file;
use std::error::Error;

type Input = ((u32, u32), (u32, u32));

fn part1(input: &[Input]) -> Result<u32, Box<dyn Error>> {
    Ok(input
        .iter()
        .map(|(a, b)| {
            if (a.0 <= b.0 && a.1 >= b.1) || (b.0 <= a.0 && b.1 >= a.1) {
                1
            } else {
                0
            }
        })
        .sum())
}

fn part2(input: &[Input]) -> Result<i32, Box<dyn Error>> {
    Ok(input
        .iter()
        .map(|(a, b)| {
            if i32::min(a.1 as i32, b.1 as i32) - i32::max(a.0 as i32, b.0 as i32) >= 0 {
                1
            } else {
                0
            }
        })
        .sum())
}

fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day4")?;
    let contents: Vec<_> = binding
        .iter()
        .map(|ln| {
            let mut pair = ln.split(',');
            let mut range_a = pair.next().unwrap().split("-");
            let mut range_b = pair.next().unwrap().split('-');
            (
                (
                    range_a.next().unwrap().parse::<u32>().unwrap(),
                    range_a.next().unwrap().parse::<u32>().unwrap(),
                ),
                (
                    range_b.next().unwrap().parse::<u32>().unwrap(),
                    range_b.next().unwrap().parse::<u32>().unwrap(),
                ),
            )
        })
        .collect();
    println!("Prob1: {}", part1(&contents)?);
    println!("Prob2: {}", part2(&contents)?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{part1, part2};
    const TEST_DATA: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";
    #[test]
    fn test1() {
        let contents: Vec<_> = TEST_DATA
            .lines()
            .map(|ln| {
                let mut pair = ln.split(',');
                let mut range_a = pair.next().unwrap().split("-");
                let mut range_b = pair.next().unwrap().split('-');
                (
                    (
                        range_a.next().unwrap().parse::<u32>().unwrap(),
                        range_a.next().unwrap().parse::<u32>().unwrap(),
                    ),
                    (
                        range_b.next().unwrap().parse::<u32>().unwrap(),
                        range_b.next().unwrap().parse::<u32>().unwrap(),
                    ),
                )
            })
            .collect();
        println!("{:?}", contents);
        assert_eq!(2, part1(&contents).unwrap());
    }

    #[test]
    fn test2() {
        let contents: Vec<_> = TEST_DATA
            .lines()
            .map(|ln| {
                let mut pair = ln.split(',');
                let mut range_a = pair.next().unwrap().split("-");
                let mut range_b = pair.next().unwrap().split('-');
                (
                    (
                        range_a.next().unwrap().parse::<u32>().unwrap(),
                        range_a.next().unwrap().parse::<u32>().unwrap(),
                    ),
                    (
                        range_b.next().unwrap().parse::<u32>().unwrap(),
                        range_b.next().unwrap().parse::<u32>().unwrap(),
                    ),
                )
            })
            .collect();
        println!("{:?}", contents);
        assert_eq!(4, part2(&contents).unwrap());
    }
}
