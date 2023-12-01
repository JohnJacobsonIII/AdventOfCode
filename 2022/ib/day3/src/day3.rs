use log::{info};
use std::collections::HashSet;

fn char_to_score(c: &char) -> usize {
    if !c.is_ascii_alphanumeric() {
        panic!("Backpack contents can only be a-z or A-Z");
    }
    let raw = (*c) as usize;
    match raw {
        // ['a', 'z'] => [1, 26]
        97..=123 => raw - 96,
        // ['A', 'Z'] => [27, 52]
        65..=91 => raw - 38,
        _ => unreachable!(),
    }
}

fn part_1_row(row: &str) -> usize {
    info!("Working on: '{}'", row);

    if row.len() % 2 == 1 {
        panic!("Backpack contents must be even");
    }

    let half_point = row.len() / 2;

    let first = &row[0..=(half_point - 1)];
    info!("First compartment: '{}'", first);

    let second = &row[half_point..];
    info!("Second compartment: '{}'", second);

    let first_set: HashSet<char> = first.chars().collect();
    let second_set: HashSet<char> = second.chars().collect();

    let common = first_set.intersection(&second_set);
    info!("Common items: {:?}", common);

    common.map( char_to_score).sum()
}

fn part_2_group(group: &[&str]) -> usize {
    let rucksack_1: HashSet<char> = group[0].chars().collect();
    let rucksack_2: HashSet<char> = group[1].chars().collect();
    let rucksack_3: HashSet<char> = group[2].chars().collect();

    let common_1_2: HashSet<char> = rucksack_1.intersection(&rucksack_2).copied().collect();
    let common_1_2_3: Vec<&char> = common_1_2.intersection(&rucksack_3).collect();
    assert!(common_1_2_3.len() == 1);

    char_to_score(common_1_2_3[0])
}

pub fn solve_day3(text: &str) -> (usize, usize) {
    let rows: Vec<&str> = text.lines().map(|l| l.trim()).collect();

    let part1 = rows.iter().map(|r| part_1_row(r)).sum();

    let part2 = rows.chunks(3).map(part_2_group).sum();

    (part1, part2)
}

#[test]
fn test_test_to_totals_zero_length() {
    assert_eq!(char_to_score(&'a'), 1);
    assert_eq!(char_to_score(&'b'), 2);
    assert_eq!(char_to_score(&'c'), 3);
    assert_eq!(char_to_score(&'d'), 4);
    assert_eq!(char_to_score(&'e'), 5);
    assert_eq!(char_to_score(&'f'), 6);
    assert_eq!(char_to_score(&'g'), 7);
    assert_eq!(char_to_score(&'h'), 8);
    assert_eq!(char_to_score(&'i'), 9);
    assert_eq!(char_to_score(&'j'), 10);
    assert_eq!(char_to_score(&'k'), 11);
    assert_eq!(char_to_score(&'l'), 12);
    assert_eq!(char_to_score(&'m'), 13);
    assert_eq!(char_to_score(&'n'), 14);
    assert_eq!(char_to_score(&'o'), 15);
    assert_eq!(char_to_score(&'p'), 16);
    assert_eq!(char_to_score(&'q'), 17);
    assert_eq!(char_to_score(&'r'), 18);
    assert_eq!(char_to_score(&'s'), 19);
    assert_eq!(char_to_score(&'t'), 20);
    assert_eq!(char_to_score(&'u'), 21);
    assert_eq!(char_to_score(&'v'), 22);
    assert_eq!(char_to_score(&'w'), 23);
    assert_eq!(char_to_score(&'x'), 24);
    assert_eq!(char_to_score(&'y'), 25);
    assert_eq!(char_to_score(&'z'), 26);
    assert_eq!(char_to_score(&'A'), 27);
    assert_eq!(char_to_score(&'B'), 28);
    assert_eq!(char_to_score(&'C'), 29);
    assert_eq!(char_to_score(&'D'), 30);
    assert_eq!(char_to_score(&'E'), 31);
    assert_eq!(char_to_score(&'F'), 32);
    assert_eq!(char_to_score(&'G'), 33);
    assert_eq!(char_to_score(&'H'), 34);
    assert_eq!(char_to_score(&'I'), 35);
    assert_eq!(char_to_score(&'J'), 36);
    assert_eq!(char_to_score(&'K'), 37);
    assert_eq!(char_to_score(&'L'), 38);
    assert_eq!(char_to_score(&'M'), 39);
    assert_eq!(char_to_score(&'N'), 40);
    assert_eq!(char_to_score(&'O'), 41);
    assert_eq!(char_to_score(&'P'), 42);
    assert_eq!(char_to_score(&'Q'), 43);
    assert_eq!(char_to_score(&'R'), 44);
    assert_eq!(char_to_score(&'S'), 45);
    assert_eq!(char_to_score(&'T'), 46);
    assert_eq!(char_to_score(&'U'), 47);
    assert_eq!(char_to_score(&'V'), 48);
    assert_eq!(char_to_score(&'W'), 49);
    assert_eq!(char_to_score(&'X'), 50);
    assert_eq!(char_to_score(&'Y'), 51);
    assert_eq!(char_to_score(&'Z'), 52);
}
