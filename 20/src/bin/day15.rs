use std::collections::HashMap;

fn sol(num_rounds: i32, numbers: &Vec<i32>) -> i32 {
    let mut spoken: HashMap<i32, i32> = numbers
        .iter()
        .enumerate()
        .map(|(i, j)| (*j, i as i32))
        .collect();
    let mut last_spoken = *numbers.iter().last().unwrap();
    for round in spoken.len() as i32 - 1..num_rounds - 1 {
        let next_num = match spoken.get(&last_spoken) {
            Some(num) => round - num,
            None => 0,
        };
        spoken.insert(last_spoken, round);

        last_spoken = next_num;
    }
    last_spoken
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Please provide a file name");
        std::process::exit(1);
    }
    let filename = args.get(1).unwrap();
    let contents = std::fs::read_to_string(filename).unwrap();
    let numbers: Vec<i32> = contents
        .split(',')
        .map(|n| n.parse::<i32>().unwrap())
        .collect();

    println!("Part 1: {}", sol(2020, &numbers));
    println!("Part 2: {}", sol(30000000, &numbers));
}
