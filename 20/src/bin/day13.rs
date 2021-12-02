use std::process::exit;
use std::str::Split;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Expected filename");
        exit(1);
    }

    let filename = args.get(1).unwrap();
    let contents = fs::read_to_string(filename).expect("Something's wrong");
    let mut lines = contents.split('\n');
    part1(&mut lines.clone());
    part2(&mut lines);
}

fn part1(lines: &mut Split<char>) {
    let arrival_time = lines.next().unwrap().parse::<i32>().unwrap();
    let ids: Vec<i32> = lines
        .next()
        .unwrap()
        .split(',')
        .filter(|c| *c != "x")
        .map(|id| id.parse::<i32>().unwrap())
        .collect();
    let wait_times = ids
        .iter()
        .map(|id| -> i32 { (arrival_time / id + 1) * id - arrival_time });
    let min_pair = ids
        .iter()
        .zip(wait_times)
        .min_by(|x, y| x.1.cmp(&y.1))
        .unwrap();
    println!("Part 1: {}", min_pair.0 * min_pair.1);
}

fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}

fn mod_inv(x: i64, n: i64) -> Option<i64> {
    let (g, x, _) = egcd(x, n);
    if g == 1 {
        Some((x % n + n) % n)
    } else {
        None
    }
}

// https://rosettacode.org/wiki/Chinese_remainder_theorem#Rust
fn chinese_remainder_theorem(residues: &Vec<i64>, modulii: &Vec<i64>) -> Option<i64> {
    let prod = modulii.iter().product::<i64>();
    let mut sum = 0;
    for (&residue, &modulus) in residues.iter().zip(modulii) {
        let p = prod / modulus;
        sum += residue * mod_inv(p, modulus)? * p;
    }
    Some(sum % prod)
}

fn part2(lines: &mut Split<char>) {
    let ids: Vec<Result<i64, _>> = lines
        .skip(1)
        .next()
        .unwrap()
        .split(',')
        .map(|id| id.parse::<i64>())
        .collect();

    let ids: Vec<(i64, i64)> = ids
        .iter()
        .zip(0..ids.len() as i64)
        .filter(|(id, _)| id.is_ok())
        .map(|(id, idx)| (*id.as_ref().unwrap(), idx))
        .collect();

    let mods = ids.iter().map(|(id, _)| *id).collect::<Vec<i64>>();
    let res = ids
        .iter()
        .map(|(id, offset)| id - offset)
        .collect::<Vec<i64>>();

    println!(
        "Part 2: {}",
        chinese_remainder_theorem(&res, &mods).unwrap()
    );
}
