use log::{debug, info, trace};

fn char_to_usize(c: &char) -> usize {
    match c {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        _ => panic!("Bad you"),
    }
}

fn text_to_first_and_last_digit(text: &str) -> (usize, usize) {
    let mut first_digit = None;
    let mut last_digit = None;

    for c in text.chars() {
        if !c.is_ascii_digit() {
            continue;
        }

        if first_digit.is_none() {
            first_digit = Some(char_to_usize(&c));
        }

        last_digit = Some(char_to_usize(&c));
    }

    let (f, l) = (first_digit.unwrap_or(0), last_digit.unwrap_or(0));
    trace!("Found: '{}' and '{}'", f, l);

    (f, l)
}

fn do_part_1(text: &str) -> usize {
    let mut sum = 0;
    debug!("Looking for secret codes");
    for line in text.lines() {
        trace!("Line: '{}'", line);

        let trimmed = line.trim();
        if trimmed.is_empty() {
            info!("Skipping empty line");
            continue;
        }

        let (tens, ones) = text_to_first_and_last_digit(line);
        let number = tens * 10 + ones;

        trace!("Number: {}", number);

        sum += number;
    }

    sum
}

fn do_part_2(text: &str) -> usize {
    let replaced = text.replace("one", "o1e")
    .replace("two", "t2o")
    .replace("three", "t3e")
    .replace("four", "4")
    .replace("five", "5e")
    .replace("six", "6")
    .replace("seven", "7n")
    .replace("eight", "e8t")
    .replace("nine", "n9e");

    do_part_1(&replaced)
}

/**
 * Answers day 1 of advent of code 2023, if possible.
 **/
pub fn solve_day_01(text: &str) -> (usize, usize) {
    let part1 = do_part_1(text);
    let part2 = do_part_2(text);

    // Reference answers:
    // Part 1: 54338
    // Part 2: 53389
    (part1, part2)
}
