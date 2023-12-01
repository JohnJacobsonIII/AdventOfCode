use std::fmt::Display;

use log::{info};

struct Interval {
    inf: usize,
    sup: usize,
}

impl Display for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}]", self.inf, self.sup)
    }
}

fn text_to_interval(text: &str) -> Interval {
    let parts: Vec<usize> = text
        .split('-')
        .map(|p| p.parse::<usize>().expect("bad interval bound"))
        .collect();
    assert!(parts.len() == 2);

    Interval {
        inf: parts[0],
        sup: parts[1],
    }
}

fn contains(a: &Interval, b: &Interval) -> bool {
    info!("{} and {} fully contain one another?", a, b);

    let mut left = a;
    let mut right = b;

    if right.inf < left.inf {
        std::mem::swap(&mut right, &mut left);
    }


    let result = right.inf == left.inf || right.sup <= left.sup;
    info!("{}", result);

    result
}

fn overlap(a: &Interval, b: &Interval) -> bool {
    info!("{} and {} overlap one another?", a, b);

    let mut left = a;
    let mut right = b;

    if right.inf < left.inf {
        std::mem::swap(&mut right, &mut left);
    }


    let result = left.sup >= right.inf;
    info!("{}", result);

    result
}

fn part_1_row(row: &str) -> usize {
    let pair: Vec<Interval> = row.split(',').map(text_to_interval).collect();
    assert!(pair.len() == 2);
    contains(&pair[0], &pair[1]) as usize
}

fn part_2_row(row: &str) -> usize {
    let pair: Vec<Interval> = row.split(',').map(text_to_interval).collect();
    assert!(pair.len() == 2);
    overlap(&pair[0], &pair[1]) as usize
}

pub fn solve_day4(text: &str) -> (usize, usize) {
    let rows: Vec<&str> = text.lines().map(|l| l.trim()).collect();

    let part1 = rows.iter().map(|l| part_1_row(l)).sum();

    let part2 = rows.iter().map(|l| part_2_row(l)).sum();

    (part1, part2)
}

#[cfg(test)]
fn test_helper(ainf: usize, asup: usize, binf: usize, bsup: usize, expected: bool) {
    let a = Interval {
        inf: ainf,
        sup: asup,
    };
    let b = Interval {
        inf: binf,
        sup: bsup,
    };
    let actual = contains(&a, &b);
    assert_eq!(actual, expected);
}

#[test]
fn test_contains() {
    test_helper(5, 7, 9, 11, false);
    test_helper(5, 7, 8, 10, false);
    test_helper(5, 7, 7, 9, false);
    test_helper(5, 7, 6, 8, false);
    test_helper(5, 7, 5, 7, true);
    test_helper(5, 7, 4, 6, false);
    test_helper(5, 7, 3, 5, false);
    test_helper(5, 7, 2, 4, false);
    test_helper(5, 7, 1, 3, false);

    test_helper(5, 7, 9, 10, false);
    test_helper(5, 7, 8, 19, false);
    test_helper(5, 7, 7, 8, false);
    test_helper(5, 7, 6, 7, true);
    test_helper(5, 7, 5, 6, true);
    test_helper(5, 7, 4, 5, false);
    test_helper(5, 7, 3, 4, false);
    test_helper(5, 7, 2, 3, false);
    test_helper(5, 7, 1, 2, false);

    test_helper(9, 11, 5, 7, false);
    test_helper(8, 10, 5, 7, false);
    test_helper(7, 9, 5, 7, false);
    test_helper(6, 8, 5, 7, false);
    test_helper(5, 7, 5, 7, true);
    test_helper(4, 6, 5, 7, false);
    test_helper(3, 5, 5, 7, false);
    test_helper(2, 4, 5, 7, false);
    test_helper(1, 3, 5, 7, false);

    test_helper(9, 10, 5, 7, false);
    test_helper(8, 19, 5, 7, false);
    test_helper(7, 8, 5, 7, false);
    test_helper(6, 7, 5, 7, true);
    test_helper(5, 6, 5, 7, true);
    test_helper(4, 5, 5, 7, false);
    test_helper(3, 4, 5, 7, false);
    test_helper(2, 3, 5, 7, false);
    test_helper(1, 2, 5, 7, false);
}
