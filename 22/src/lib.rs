use std::{error::Error, fmt, fs};

#[derive(Debug, Clone)]
pub struct ProblemError;

impl fmt::Display for ProblemError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unable to find solution for problem")
    }
}

impl Error for ProblemError {}

pub fn read_file(fname: &str) -> Result<Vec<String>, Box<dyn Error>> {
    Ok(fs::read_to_string(fname)?
        .lines()
        .map(String::from)
        .collect())
}
