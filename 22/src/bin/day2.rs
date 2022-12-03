use aoc22::read_file;
use std::error::Error;

type Input = (char, char);

fn part1(input: &[Input]) -> Result<i32, Box<dyn Error>> {
    Ok(input
        .iter()
        .map(|(op, mine)| match mine {
            'X' => match op {
                'A' => 1 + 3,
                'B' => 1 + 0,
                'C' => 1 + 6,
                _ => 0,
            },
            'Y' => match op {
                'A' => 2 + 6,
                'B' => 2 + 3,
                'C' => 2+ 0,
                _ => 0,
            },
            'Z' => match op {
                'A' => 3 + 0,
                'B' => 3 + 6,
                'C' => 3 + 3,
                _ => 0
            },
            _ => 0
        })
        .sum())
}

fn part2(input: &[Input]) -> Result<i32, Box<dyn Error>> {
    Ok(input
        .iter()
        .map(|(op, mine)| match mine {
            'X' => match op {
                'A' => 3 + 0,
                'B' => 1 + 0,
                'C' => 2 + 0,
                _ => 0,
            },
            'Y' => match op {
                'A' => 1 + 3,
                'B' => 2 + 3,
                'C' => 3 + 3,
                _ => 0,
            },
            'Z' => match op {
                'A' => 2 + 6,
                'B' => 3 + 6,
                'C' => 1 + 6,
                _ => 0
            },
            _ => 0
        })
        .sum())
}

fn main() -> Result<(), Box<dyn Error>> {
    let contents: Vec<_> = read_file("res/day2")?
        .iter()
        .map(|ln| {
            let mut s = ln.split(' ');
            (
                s.next().unwrap().chars().next().unwrap(),
                s.next().unwrap().chars().next().unwrap(),
            )
        })
        .collect();
    println!("Prob1: {}", part1(&contents)?);
    println!("Prob2: {}", part2(&contents)?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::part2;
    const TEST_DATA: &str = "A Y\nB X\nC Z";
    #[test]
    fn test2() {
        let contents: Vec<_> = TEST_DATA.lines()
        .map(|ln| {
            let mut s = ln.split(' ');
            (
                s.next().unwrap().chars().next().unwrap(),
                s.next().unwrap().chars().next().unwrap(),
            )
        })
        .collect();
        assert_eq!(12, part2(&contents).unwrap());

    }
}
