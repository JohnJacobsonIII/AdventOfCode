use std::collections::HashSet;



fn solve_part1(text: &str) -> Option<usize> {
    for (index, window) in text.chars().collect::<Vec<char>>().windows(4).enumerate() {
        let set: HashSet<char> = HashSet::from_iter(window.iter().cloned());
        if set.len() == 4 {
            return Some(index + 4);
        }
    }
    None
}

fn solve_part2(text: &str) -> Option<usize> {
    for (index, window) in text.chars().collect::<Vec<char>>().windows(14).enumerate() {
        let set: HashSet<char> = HashSet::from_iter(window.iter().cloned());
        if set.len() == 14 {
            return Some(index + 14);
        }
    }
    None
}

pub fn solve_day6(text: &str) -> (usize, usize) {
    let part1 = solve_part1(text).unwrap();
    let part2 = solve_part2(text).unwrap();
    (part1, part2)
}
