use log::{debug, trace};

type Stacks = Vec<Vec<char>>;
type Movement = (usize, usize, usize);

fn parse_text(text: &str) -> (Stacks, Vec<Movement>) {
    let mut parts = text.split("\n\n");

    let start_text = parts.next().unwrap();
    let moves_text = parts.next().unwrap();
    assert!(parts.next().is_none());

    let mut start_lines = start_text.lines().rev();
    let index_line = start_lines.next().unwrap();
    debug!("Stack index line: '{}'", index_line);
    let indices: Vec<usize> = index_line
        .split_whitespace()
        .map(|t| t.parse::<usize>().unwrap())
        .collect();
    trace!("  split into: {:?}", indices);
    let max_index = indices[indices.len() - 1];
    assert_eq!(indices, (1..=max_index).collect::<Vec<usize>>());

    let mut stacks: Stacks = vec![];
    for _ in 1..=max_index {
        stacks.push(vec![]);
    }

    for line in start_lines {
        for (index, part) in line.chars().collect::<Vec<char>>().chunks(4).enumerate() {
            if part.iter().all(|c| *c == ' ') {
                trace!("No item for stack {}", index);
            } else {
                assert_eq!(part[0], '[');
                assert_eq!(part[2], ']');
                let item = part[1];
                trace!("Put '{}' on stack {}" , item, index);
                stacks[index].push(part[1]);
            }
        }
    }

    let mut moves = vec![];
    for line in moves_text.lines() {
        let mut parts = line.split_whitespace();
        let word_0 = parts.next().unwrap();
        assert_eq!("move", word_0);
        let count = parts.next().unwrap().parse::<usize>().unwrap();
        let word_1 = parts.next().unwrap();
        assert_eq!("from", word_1);
        let source = parts.next().unwrap().parse::<usize>().unwrap();
        let word_2 = parts.next().unwrap();
        assert_eq!("to", word_2);
        let dest = parts.next().unwrap().parse::<usize>().unwrap();
        assert!(parts.next().is_none());
        moves.push((count, source, dest));
    }

    (stacks, moves)
}

fn do_move_part1(stacks: &mut Stacks, count: &usize, source: &usize, dest: &usize) {
    for _ in 0..*count {
        let moved = stacks[source-1].pop().unwrap();
        stacks[dest-1].push(moved);
    }
}

fn simulate_part1(stacks: &Stacks, moves: &Vec<Movement>) -> String {
    let mut stacks = stacks.clone();
    for (count, source, dest) in moves {
        do_move_part1(&mut stacks, count, source, dest);
    }
    stacks.iter().map(|s| s.last().unwrap_or(&' ')).collect()
}

fn do_move_part2(stacks: &mut Stacks, count: &usize, source: &usize, dest: &usize) {
    let mut temp = vec![];
    for _ in 0..*count {
        temp.push(stacks[source-1].pop().unwrap());
    }
    for _ in 0..*count {
        stacks[dest-1].push(temp.pop().unwrap());
    }
}

fn simulate_part2(stacks: &Stacks, moves: &Vec<Movement>) -> String {
    let mut stacks = stacks.clone();
    for (count, source, dest) in moves {
        do_move_part2(&mut stacks, count, source, dest);
    }
    stacks.iter().map(|s| s.last().unwrap_or(&' ')).collect()
}

pub fn solve_day5(text: &str) -> (String, String) {
    let (start, moves) = parse_text(text);

    let part1 = simulate_part1(&start, &moves);
    let part2 = simulate_part2(&start, &moves);

    (part1, part2)
}
