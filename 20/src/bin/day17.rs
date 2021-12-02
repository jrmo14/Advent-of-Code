fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Please provide a file name");
        std::process::exit(1);
    }

    let filename = args.get(1).unwrap();
    let contents = std::fs::read_to_string(filename).unwrap();
    let mut blocks: Vec<Vec<Vec<bool>>> = Vec::new();
    blocks.push(
        contents
            .split('\n')
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        '#' => true,
                        _ => false,
                    })
                    .collect::<Vec<bool>>()
            })
            .collect::<Vec<Vec<bool>>>(),
    );

}
