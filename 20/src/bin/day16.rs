use std::collections::hash_map::{Entry, RandomState};
use std::collections::{HashMap, HashSet};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Please provide a file name");
        std::process::exit(1);
    }

    let filename = args.get(1).unwrap();
    let contents = std::fs::read_to_string(filename).unwrap();
    let blocks: Vec<String> = contents.split("\n\n").map(str::to_string).collect();

    let rules: Vec<(String, (i32, i32), (i32, i32))> = blocks[0]
        .split('\n')
        .map(|line| {
            let mut splitr = line.split(':');
            let rule_name = splitr.next().unwrap();
            let mut parings = splitr
                .next()
                .unwrap()
                .strip_prefix(' ')
                .unwrap()
                .split(" or ");
            let lower: Vec<i32> = parings
                .next()
                .unwrap()
                .split('-')
                .map(str::parse::<i32>)
                .map(Result::unwrap)
                .collect();
            let upper: Vec<i32> = parings
                .next()
                .unwrap()
                .split('-')
                .map(str::parse::<i32>)
                .map(Result::unwrap)
                .collect();
            (
                rule_name.to_string(),
                (lower[0], lower[1]),
                (upper[0], upper[1]),
            )
        })
        .collect();

    let my_ticket: Vec<i32> = blocks[1]
        .split('\n')
        .nth(1)
        .unwrap()
        .split(',')
        .map(str::parse::<i32>)
        .map(Result::unwrap)
        .collect();

    let other_tickets: Vec<Vec<i32>> = blocks[2]
        .split('\n')
        .skip(1)
        .map(|line| {
            line.split(',')
                .map(str::parse::<i32>)
                .map(Result::unwrap)
                .collect()
        })
        .collect();

    println!(
        "Part 1: {}",
        part1(
            &rules
                .iter()
                .map(|(_, (a, b), (c, d))| ((*a, *b), (*c, *d)))
                .collect(),
            &my_ticket,
            &other_tickets,
        )
    );

    println!("Part 2: {}", part2(&rules, &my_ticket, &other_tickets));
}

fn part1(
    rules: &Vec<((i32, i32), (i32, i32))>,
    _: &Vec<i32>,
    other_tickets: &Vec<Vec<i32>>,
) -> i32 {
    let bound_check = |num: i32, bound: (i32, i32)| -> bool { num >= bound.0 && num <= bound.1 };
    let apply_rule = |num: i32, rule: ((i32, i32), (i32, i32))| -> bool {
        bound_check(num, rule.0) || bound_check(num, rule.1)
    };

    let mut count = 0;
    for ticket in other_tickets {
        for &value in ticket {
            if !rules.iter().map(|&rule| apply_rule(value, rule)).any(|x| x) {
                count += value
            }
        }
    }
    count
}

fn part2(
    rules: &Vec<(String, (i32, i32), (i32, i32))>,
    my_ticket: &Vec<i32>,
    other_tickets: &Vec<Vec<i32>>,
) -> usize {
    let bound_check = |num: i32, bound: (i32, i32)| -> bool { num >= bound.0 && num <= bound.1 };
    let apply_rule = |num: i32, rule: ((i32, i32), (i32, i32))| {
        bound_check(num, rule.0) || bound_check(num, rule.1)
    };
    let valid_tickets: Vec<&Vec<i32>> = other_tickets
        .into_iter()
        .filter(|&ticket| {
            ticket
                .iter()
                .all(|&value| rules.iter().any(|rule| apply_rule(value, (rule.1, rule.2))))
        })
        .collect();

    let ticket_len = my_ticket.len();
    let mut rule_matches: HashMap<String, HashSet<usize>> = HashMap::new();
    for rule in rules {
        for i in 0..ticket_len {
            if valid_tickets
                .iter()
                .all(|ticket| apply_rule(ticket[i], (rule.1, rule.2)))
            {
                match rule_matches.entry(rule.0.clone()) {
                    Entry::Occupied(rules_pos) => {
                        rules_pos.into_mut().insert(i);
                    }
                    Entry::Vacant(rules_pos) => {
                        let mut new_set = HashSet::new();
                        new_set.insert(i);
                        rules_pos.insert(new_set);
                    }
                }
            }
        }
    }

    while rule_matches
        .values()
        .any(|possibilities| possibilities.len() != 1)
    {
        let keys: Vec<String> = rule_matches.keys().map(|key| key.clone()).collect();
        for key in &keys {
            if rule_matches.get(&*key).unwrap().len() == 1 {
                let lockdown_value = *rule_matches.get(&*key).unwrap().iter().nth(0).unwrap();
                for rule_name in &keys {
                    if rule_name.eq(key) {
                        continue;
                    }
                    rule_matches
                        .get_mut(&*rule_name)
                        .unwrap()
                        .remove(&lockdown_value);
                }
            }
        }
    }
    let dep_idxs = rule_matches
        .iter()
        .filter_map(|(name, val)| match name.starts_with("departure") {
            true => Some(*(val.iter().nth(0).unwrap())),
            false => None,
        })
        .collect::<Vec<usize>>();
    let mut prod: usize = 1;
    for idx in dep_idxs {
        prod *= my_ticket[idx] as usize;
    }
    prod
}
