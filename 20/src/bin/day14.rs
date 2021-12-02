use std::collections::HashMap;

fn part1_mask(mut value: i64, ones_mask: i64, zeros_mask: i64) -> i64 {
    value |= ones_mask;
    value = !value;
    value |= zeros_mask;
    !value
}

fn part1(instructions: &Vec<String>) -> i64 {
    let mut mem = HashMap::<i64, i64>::new();
    let mut ones_mask: i64 = 0;
    let mut zeros_mask: i64 = 0;
    for instruction in instructions {
        match &instruction.chars().nth(1).unwrap() {
            'e' => {
                let address = instruction
                    .split(&['[', ']'][..])
                    .nth(1)
                    .unwrap()
                    .parse::<i64>()
                    .unwrap();
                let mut value = instruction
                    .split(' ')
                    .nth(2)
                    .unwrap()
                    .parse::<i64>()
                    .unwrap();

                mem.insert(address, part1_mask(value, ones_mask, zeros_mask));
            }
            'a' => {
                zeros_mask = 0;
                ones_mask = 0;
                let mask = instruction
                    .split('=')
                    .nth(1)
                    .unwrap()
                    .strip_prefix(' ')
                    .unwrap();
                for (i, c) in mask.chars().rev().enumerate() {
                    match c {
                        '1' => {
                            ones_mask |= 1 << i;
                        }
                        '0' => {
                            zeros_mask |= 1 << i;
                        }
                        'X' => {}
                        _ => unreachable!(),
                    }
                }
            }
            _ => unreachable!(),
        }
    }
    mem.values().sum()
}

fn build_addr(address: i64, mask_str: &str, idx: i64) -> Vec<i64> {
    let mut addrs = Vec::<i64>::new();
    match mask_str.chars().nth(idx as usize) {
        Some('0') => addrs.append(&mut build_addr(address, mask_str, idx + 1)),
        Some('1') => addrs.append(&mut build_addr(
            address | (1 << (35 - idx)),
            mask_str,
            idx + 1,
        )),
        Some('X') => {
            addrs.append(&mut build_addr(address, mask_str, idx + 1));
            addrs.append(&mut build_addr(
                address ^ (1 << (35 - idx)),
                mask_str,
                idx + 1,
            ));
        }
        _ => addrs.push(address),
    }
    addrs
}

fn part2(instructions: &Vec<String>) -> i64 {
    let mut mem = HashMap::<i64, i64>::new();
    let mut addresses = Vec::<i64>::new();
    let mut mask_str = "";
    for instruction in instructions {
        match &instruction.chars().nth(1).unwrap() {
            'e' => {
                let address = instruction
                    .split(&['[', ']'][..])
                    .nth(1)
                    .unwrap()
                    .parse::<i64>()
                    .unwrap();
                let value = instruction
                    .split(' ')
                    .nth(2)
                    .unwrap()
                    .parse::<i64>()
                    .unwrap();
                let addrs = build_addr(address, mask_str, 0);
                for addr in addrs {
                    mem.insert(addr, value);
                }
            }
            'a' => {
                mask_str = instruction.split(' ').nth(2).unwrap();
            }
            _ => unreachable!(),
        }
    }
    mem.values().sum()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Please provide a file name");
        std::process::exit(1);
    }
    let filename = args.get(1).unwrap();
    let contents = std::fs::read_to_string(filename).unwrap();
    let lines: Vec<String> = contents.split('\n').map(str::to_string).collect();
    println!("Part 1: {}", part1(&lines));
    println!("Part 2: {}", part2(&lines));
}
