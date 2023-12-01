use log::{debug, info, trace, warn};

/**
 * Totals values for the following format:
 *   + Zero or one positive base ten integer per line.
 *   + Consecutive integer lines constitute an entry.
 *   + Entries are separated by blank lines.
 *
 * Corner cases:
 *  * extra whitespace on any line
 *    + ignore
 *  * multiple blank lines
 *    + they count as a single separator
 *  * non positive base ten integers
 *    + error
 **/
fn text_to_totals(text: &str) -> Vec<usize> {
    let mut totals = vec![];
    let mut entry_total = 0;
    let mut in_entry = false;
    debug!("Looking for calories for Elf 1");
    for line in text.lines() {
        trace!("Line: '{}'", line);

        let trimmed = line.trim();
        if trimmed.is_empty() {
            // Only make a new total when we first see a blank line
            if in_entry {
                // An empty line means we have finished an entry
                info!("Elf {} has calorie total {}", totals.len() + 1, entry_total);
                totals.push(entry_total);

                // ... and started a new one
                debug!("Looking for calories for elf {}", totals.len() + 1);
                entry_total = 0;
                in_entry = false;
            }
        } else {
            // A line with a number increases the entry's total
            debug!("Found {}", trimmed);
            let value = trimmed.parse::<usize>().expect("not a positive integer");
            entry_total += value;
            in_entry = true;
        }
    }

    // We may have ended with an entry present
    if in_entry {
        info!("Elf {} has calorie total {}", totals.len() + 1, entry_total);
        totals.push(entry_total);
    } else {
        debug!("There was no elf {}", totals.len() + 1);
    }
    totals
}

/**
 * Answers day 1 of advent of code 2022, if possible.
 * At least one elf must be present to answer part one.
 * At least three elves must be present to answer part three.
 **/
pub fn solve_day1(text: &str) -> (usize, usize) {
    // Sorting is inefficient for the particular problem,
    // but is fast enough and allow easy changes, e.g. "Now give me the top 4"
    let mut totals = text_to_totals(text);
    totals.sort();
    totals.reverse();

    // Top value
    let part1 = if totals.is_empty() {
        warn!("No elves, cannot answer part 1!");
        0
    } else {
        totals[0]
    };

    // Sum of top three values
    let part2 = if totals.len() <= 2 {
        warn!("Less than three elves, cannot answer part 2!");
        0
    } else {
        totals[0..=2].iter().sum()
    };

    (part1, part2)
}

#[cfg(test)]
fn test_helper(text: &str, expected_totals: &Vec<usize>) {
    let actual_totals = text_to_totals(text);
    assert_eq!(actual_totals.len(), expected_totals.len());
    assert_eq!(&actual_totals, expected_totals);
}

#[test]
fn test_test_to_totals_zero_length() {
    test_helper("", &vec![]);
    test_helper("  ", &vec![]);
    test_helper("\t", &vec![]);
    test_helper("\n", &vec![]);
}

#[test]
fn test_test_to_totals_one_length() {
    test_helper("42", &vec![42]);
    test_helper("  42", &vec![42]);
    test_helper("42   ", &vec![42]);
    test_helper(" 42 ", &vec![42]);
    test_helper("42\t ", &vec![42]);

    test_helper("24\n1", &vec![25]);
    test_helper("1\n  24", &vec![25]);
    test_helper("\n\n24   \n1", &vec![25]);
    test_helper(" 24\n  1\n\n\n ", &vec![25]);
}

#[test]
fn test_test_to_totals_two_length() {
    test_helper("24\n\n25", &vec![24, 25]);
    test_helper("24\n\n\n\n\n25", &vec![24, 25]);
    test_helper("\n\n24\n\n25\n", &vec![24, 25]);
    test_helper("24\n\n24\n1", &vec![24, 25]);
}
