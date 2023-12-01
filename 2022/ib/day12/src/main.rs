use clap::Parser;
use log::{info, Level};
use std::{fs, path::PathBuf};

mod day12;
use day12::solve_day12;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// File containing the input
    #[arg()]
    filename: PathBuf,

    /// One of {off, error, warn, info, debug, trace}
    #[arg(long, short)]
    verbosity: Option<Level>,
}

fn main() {
    let args = Args::parse();

    let log_level = args.verbosity.unwrap_or(Level::Warn);
    simple_logger::init_with_level(log_level).expect("unable to set log level");

    let filepath = args.filename.as_path();
    info!("Reading: {}", filepath.to_string_lossy());
    let text = fs::read_to_string(filepath).expect("unable to read file");

    let (part1, part2) = solve_day12(&text);

    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
