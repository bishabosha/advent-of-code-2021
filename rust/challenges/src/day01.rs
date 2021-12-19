use std::cmp::Ordering;

pub fn read_ints(input: String) -> Result<Vec<i32>, String> {
    type Parsed = Result<Vec<i32>, std::num::ParseIntError>;
    let ints: Parsed = input.lines().map(|line| line.parse::<i32>()).collect();
    return ints.or_else(|err| Err(format!("{}", err)));
}

pub fn part1(input: Vec<i32>) -> i32 {
    return run(1, input);
}

pub fn part2(input: Vec<i32>) -> i32 {
    return run(3, input);
}

fn run(n: usize, input: Vec<i32>) -> i32 {
    return input
        .iter()
        .zip(input.iter().skip(n))
        .filter(|(a, b)| a.cmp(b) == Ordering::Less)
        .count() as i32;
}