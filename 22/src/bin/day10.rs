use aoc22::read_file;
use std::error::Error;

#[derive(Debug)]
enum Instruction {
    NOP,
    ADDX(i64),
}

fn part1(input: &[Instruction]) -> i64 {
    let mut x = 1;
    let mut str = 0;
    let mut cycles = 0;

    let compute_strength = |mut cycles: i64, mut str: i64, x: i64| {
        cycles += 1;
        match cycles {
            20 | 60 | 100 | 140 | 180 | 220 => {
                str += cycles * x;
            }
            _ => {}
        }
        (cycles, str)
    };

    for instr in input {
        match instr {
            Instruction::NOP => (cycles, str) = compute_strength(cycles, str, x),
            Instruction::ADDX(i) => {
                (cycles, str) = compute_strength(cycles, str, x);
                (cycles, str) = compute_strength(cycles, str, x);
                x += i;
            }
        };
    }
    str
}

fn part2(input: &[Instruction]) {
    let mut cycles: i64 = 0;
    let mut x = 1;
    let print = |mut cycles: i64, x: i64| {
        if ((cycles % 40) - x).abs() <= 1 {
            print!("#");
            cycles += 1;
        } else {
            print!(".");
            cycles += 1;
        }
        if cycles % 40 == 0 {
            println!()
        }
        cycles
    };
    for instr in input {
        match instr {
            Instruction::NOP => cycles = print(cycles, x),
            Instruction::ADDX(i) => {
                cycles = print(cycles, x);
                cycles = print(cycles, x);
                x += i;
            }
        }
    }
}

fn parse(input: &[&str]) -> Vec<Instruction> {
    input
        .into_iter()
        .map(|&instr| {
            let mut i = instr.split(' ');
            let op = i.next().unwrap();
            match op {
                "addx" => Instruction::ADDX(i.next().unwrap().parse::<i64>().unwrap()),
                _ => Instruction::NOP,
            }
        })
        .collect()
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day10")?;
    let input: Vec<_> = parse(&binding.iter().map(|ln| &**ln).collect::<Vec<_>>());
    println!("Part 1: {}", part1(&input));
    println!("Part 2\n");
    part2(&input);

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{parse, part1};

    #[test]
    fn test1() {
        let input = parse(&TEST_DATA.lines().map(|ln| ln).collect::<Vec<_>>());
        assert_eq!(13140, part1(&input));
    }

    const TEST_DATA: &str = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";
}
