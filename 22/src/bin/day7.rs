use aoc22::{read_file, ProblemError};
use std::{cell::RefCell, collections::HashMap, error::Error, rc::Rc};

type Input = Rc<RefCell<Node>>;
type Output = usize;

#[derive(Clone, PartialEq)]
struct Node {
    pub name: String,
    pub size: usize,
    pub children: HashMap<String, Rc<RefCell<Node>>>,
    pub parent: Option<Rc<RefCell<Node>>>,
}

impl Node {
    pub fn new(name: String) -> Self {
        Self {
            name,
            size: 0,
            children: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(name: String, parent: Rc<RefCell<Node>>) -> Self {
        Self {
            name,
            size: 0,
            children: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn create_child(&mut self, name: String, parent_celld_ref: Rc<RefCell<Node>>) {
        self.add_child(
            name.clone(),
            Rc::new(RefCell::new(Self::with_parent(name, parent_celld_ref))),
        );
    }

    pub fn add_child(&mut self, name: String, child: Rc<RefCell<Node>>) {
        self.children.insert(name, child);
    }

    pub fn compute_size(&self) -> usize {
        self.size
            + self
                .children
                .values()
                .map(|child| child.borrow().compute_size())
                .sum::<usize>()
    }
}

fn part1(input: &Input) -> Result<Output, Box<dyn Error>> {
    Ok(input
        .borrow()
        .children
        .values()
        .filter_map(|child| {
            let child_size = child.borrow().compute_size();
            let inner_size = part1(child).unwrap();
            // Want a sum of all the _directory_ sizes
            // Don't count straight up files to the count, just if they're in a directory

            // We've a file
            if child.borrow().children.is_empty() {
                None
            } else if child_size > 100000 {
                Some(inner_size)
            } else {
                Some(child_size + inner_size)
            }
        })
        .sum())
}

fn part2_helper(cur: &Input, size_to_free: usize) -> Option<usize> {
    // If we don't have kids we dont count
    if cur.borrow().children.is_empty() {
        return None;
    }

    // Check if our current node is valid
    let cur_size = cur.borrow().compute_size();
    // If our cur node is too small, then so are all it's children
    if cur_size >= size_to_free {
        let min_child = cur
            .borrow()
            .children
            .values()
            .filter_map(|child| {
                let child_size = child.borrow().compute_size();
                if child_size >= size_to_free {
                    // See if there's a child that is closer to our target
                    part2_helper(child, size_to_free)
                } else {
                    None
                }
            })
            .min();
        // If we've a valid child, it must be less than or equal in size to us,
        // and also greater than our target
        if min_child.is_some() {
            min_child
        } else {
            // Guess we're it
            Some(cur_size)
        }
    } else {
        // None of the options work here
        None
    }
}

fn part2(input: &Input) -> Result<usize, Box<dyn Error>> {
    let to_delete = 30000000 - (70000000 - input.borrow().compute_size());
    println!("Looking to free {to_delete}");
    match part2_helper(input, to_delete) {
        Some(size) => {
            println!("Freeing {size}");
            Ok(size)
        }
        None => Err(Box::new(ProblemError {})),
    }
}

fn parse(input: &[&str]) -> Input {
    let root = Rc::new(RefCell::new(Node::new(String::from("/"))));
    let mut current = Rc::clone(&root);
    // Skip the first one cuz we know it's root...
    for line in input.iter().skip(1) {
        if line.starts_with("$ ls") {
            // IDK
        } else if line.starts_with("$ cd") {
            if line.starts_with("$ cd ..") {
                let parent_binding = Rc::clone(current.borrow().parent.as_ref().unwrap());
                current = parent_binding;
            } else {
                let target_child = &line[5..].to_owned();
                let next_binding = Rc::clone(current.borrow().children.get(target_child).unwrap());
                current = next_binding;
            }
        } else if let Some(child_name) = line.strip_prefix("dir ") {
            let child_name = child_name.to_owned();
            current
                .borrow_mut()
                .create_child(child_name, Rc::clone(&current));
        } else {
            // Assume it's a file here
            let mut line_iter = line.split(' ');
            let size: usize = line_iter.next().unwrap().parse::<usize>().unwrap();
            let name = line_iter.next().unwrap().to_owned();
            let mut file = Node::new(name.clone());
            file.size = size;
            current
                .borrow_mut()
                .add_child(name, Rc::new(RefCell::new(file)))
        }
    }
    root
}

fn main() -> Result<(), Box<dyn Error>> {
    let binding = read_file("res/day7")?;
    let input: Vec<_> = binding.iter().map(|s| &**s).collect();
    let input = parse(&input);
    println!("Part1: {}", part1(&input)?);
    println!("Part2: {}", part2(&input)?);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{parse, part1, part2};

    const TEST_DATA: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    #[test]
    fn test1() {
        let input: Vec<_> = TEST_DATA.lines().map(|s| &*s).collect();
        let input = parse(&*input);
        assert_eq!(95437, part1(&input).unwrap());
    }
    #[test]
    fn test2() {
        let input: Vec<_> = TEST_DATA.lines().map(|s| &*s).collect();
        let input = parse(&*input);
        assert_eq!(24933642, part2(&input).unwrap())
    }
}
