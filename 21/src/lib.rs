use std::{env, error::Error, fs};

pub fn read_file() -> Result<Vec<String>, Box<dyn Error>> {
    let args: Vec<_> = env::args().collect();
    let fname = args.get(1).ok_or("Need filename argument")?;
    Ok(fs::read_to_string(fname)?
        .split("\n")
        .map(String::from)
        .filter(|s| !s.is_empty())
        .collect())
}
