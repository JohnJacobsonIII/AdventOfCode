enum Shape {
    Rock,
    Paper,
    Scissors,
}

enum Outcome {
    Loose,
    Draw,
    Win,
}

fn shape_to_score(shape: &Shape) -> usize {
    match shape {
        Shape::Rock => 1,
        Shape::Paper => 2,
        Shape::Scissors => 3,
    }
}

fn outcome_to_score(outcome: &Outcome) -> usize {
    match outcome {
        Outcome::Loose => 0,
        Outcome::Draw => 3,
        Outcome::Win => 6,
    }
}

fn column_0_to_shape(item: &str) -> Shape {
    match item {
        "A" => Shape::Rock,
        "B" => Shape::Paper,
        "C" => Shape::Scissors,
        _ => panic!("Opponent can only play one of {{A, B, C}}"),
    }
}

fn column_1_to_shape(item: &str) -> Shape {
    match item {
        "X" => Shape::Rock,
        "Y" => Shape::Paper,
        "Z" => Shape::Scissors,
        _ => panic!("I can only play one of {{X, Y, Z}}"),
    }
}

fn column_1_to_outcome(item: &str) -> Outcome {
    match item {
        "X" => Outcome::Loose,
        "Y" => Outcome::Draw,
        "Z" => Outcome::Win,
        _ => panic!("I can only have an outcome of {{X, Y, Z}}"),
    }
}

fn determine_game(opponent: &Shape, me: &Shape) -> Outcome {
    match (opponent, me) {
        // I win
        (Shape::Rock, Shape::Paper) => Outcome::Win,
        (Shape::Paper, Shape::Scissors) => Outcome::Win,
        (Shape::Scissors, Shape::Rock) => Outcome::Win,

        // We draw
        (Shape::Rock, Shape::Rock) => Outcome::Draw,
        (Shape::Paper, Shape::Paper) => Outcome::Draw,
        (Shape::Scissors, Shape::Scissors) => Outcome::Draw,

        // Opponent wins
        (Shape::Rock, Shape::Scissors) => Outcome::Loose,
        (Shape::Paper, Shape::Rock) => Outcome::Loose,
        (Shape::Scissors, Shape::Paper) => Outcome::Loose,
    }
}

fn post_determine_game(opponent: &Shape, outcome: &Outcome) -> Shape {
    match (opponent, outcome) {
        // Play rock
        (Shape::Rock, Outcome::Draw) => Shape::Rock,
        (Shape::Paper, Outcome::Loose) => Shape::Rock,
        (Shape::Scissors, Outcome::Win) => Shape::Rock,

        // Play paper
        (Shape::Rock, Outcome::Win) => Shape::Paper,
        (Shape::Paper, Outcome::Draw) => Shape::Paper,
        (Shape::Scissors, Outcome::Loose) => Shape::Paper,

        // Play scissors
        (Shape::Rock, Outcome::Loose) => Shape::Scissors,
        (Shape::Paper, Outcome::Win) => Shape::Scissors,
        (Shape::Scissors, Outcome::Draw) => Shape::Scissors,
    }
}

fn score_game(opponent: &Shape, me: &Shape) -> usize {
    let shape_score = shape_to_score(me);

    let outcome = determine_game(opponent, me);
    let outcome_score = outcome_to_score(&outcome);

    shape_score + outcome_score
}

fn text_to_rows(text: &str) -> Vec<(&str, &str)> {
    let mut rows = vec![];
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let row: Vec<&str> = trimmed.split_whitespace().collect();
        if row.len() != 2 {
            panic!("Each line can only have 2 items!");
        }

        rows.push((row[0], row[1]));
    }
    rows
}

fn part_1_row(row: &(&str, &str)) -> usize {
    let opponent = column_0_to_shape(row.0);
    let me = column_1_to_shape(row.1);
    score_game(&opponent, &me)
}

fn part_2_row(row: &(&str, &str)) -> usize {
    let opponent = column_0_to_shape(row.0);
    let outcome = column_1_to_outcome(row.1);
    let me = post_determine_game(&opponent, &outcome);
    score_game(&opponent, &me)
}

pub fn solve_day2(text: &str) -> (usize, usize) {
    let rows = text_to_rows(text);

    if rows.is_empty() {
        (0, 0)
    } else {
        (
            rows.iter().map(part_1_row).sum(),
            rows.iter().map(part_2_row).sum(),
        )
    }
}
