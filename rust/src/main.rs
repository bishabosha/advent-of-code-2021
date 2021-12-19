use challenges::runner;
use challenges::day01;

fn main() {
    runner::run_challenge("day01", "part1", day01::read_ints, day01::part1);
    runner::run_challenge("day01", "part2", day01::read_ints, day01::part2);
}
