use aoc22::read_file;
use std::error::Error;

#[derive(Clone, Copy)]
struct Instruction {
    count: u32,
    src: u32,
    dest: u32,
}

struct Input {
    stacks: Vec<Vec<char>>,
    instructions: Vec<Instruction>,
}

fn part1(input: &Input) -> Result<String, Box<dyn Error>> {
    let mut stacks = input.stacks.clone();
    for instr in &input.instructions {
        for _ in 0..instr.count {
            let v = stacks.get_mut(instr.src as usize).unwrap().pop().unwrap();
            stacks.get_mut(instr.dest as usize).unwrap().push(v);
        }
    }
    let mut rv = String::new();
    for stack in &mut stacks {
        rv.push(stack.pop().unwrap())
    }
    Ok(rv)
}

fn part2(input: &Input) -> Result<String, Box<dyn Error>> {
    let mut stacks = input.stacks.clone();
    for instr in &input.instructions {
        let mut to_add = Vec::with_capacity(instr.count as usize);
        for _ in 0..instr.count {
            to_add.push(stacks.get_mut(instr.src as usize).unwrap().pop().unwrap());
        }
        stacks
            .get_mut(instr.dest as usize)
            .unwrap()
            .extend(to_add.iter().rev());
    }
    let mut rv = String::new();
    for stack in &mut stacks {
        rv.push(stack.pop().unwrap())
    }
    Ok(rv)
}

fn parse(lines: Vec<String>) -> Input {
    let mut instructions = Vec::new();
    let mut stacks = Vec::new();

    let mut parsing_instrs = false;
    for ln in lines {
        // Line with numbers tells us nothing....
        if ln.starts_with(" 1") {
            continue;
        }
        // Empty line is just before instructions
        if ln.is_empty() {
            parsing_instrs = true;
            continue;
        }
        if parsing_instrs {
            let ln: Vec<_> = ln
                .split(' ')
                .filter_map(|word| word.parse::<u32>().ok())
                .collect();
            instructions.push(Instruction {
                count: ln[0],
                src: ln[1] - 1,
                dest: ln[2] - 1,
            });
        } else {
            for (i, c) in ln.chars().skip(1).step_by(4).enumerate() {
                if stacks.len() <= i {
                    stacks.push(Vec::new());
                }
                if c != ' ' {
                    let cur_stack = stacks.get_mut(i).unwrap();
                    cur_stack.insert(0, c);
                }
            }
        }
    }

    Input {
        stacks: stacks.clone(),
        instructions: instructions.clone(),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day5")?;

    let input = parse(binding);
    println!("Part1: {}", part1(&input)?);
    println!("Part2: {}", part2(&input)?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{parse, part1, part2};
    const TEST_DATA: &str = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";
    #[test]
    fn test1() {
        let contents = parse(TEST_DATA.lines().map(|a| String::from(a)).collect::<_>());
        assert_eq!(String::from("CMZ"), part1(&contents).unwrap());
    }

    //#[test]
    fn test2() {
        let contents = parse(TEST_DATA.lines().map(|a| String::from(a)).collect::<_>());
        assert_eq!(String::from("CMZ"), part1(&contents).unwrap());
    }
}
