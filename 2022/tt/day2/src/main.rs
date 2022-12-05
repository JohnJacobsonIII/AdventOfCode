use std::env;
use std::fs;

fn play_game1(opponents_hand: &str, my_hand: &str) -> u32 {
    match opponents_hand {
        "A" => match my_hand {
            "X" => 4,
            "Y" => 8,
            "Z" => 3,
            _ => 0

        },
        "B" => match my_hand {
            "X" => 1,
            "Y" => 5,
            "Z" => 9,
            _ => 0
        },
        "C" => match my_hand {
            "X" => 7,
            "Y" => 2,
            "Z" => 6,
            _ => 0
        },
        _ => 0
    }
}

fn play_game2(opponents_hand: &str, my_hand: &str) -> u32 {
    match opponents_hand {
        "A" => match my_hand {
            "X" => 3,
            "Y" => 4,
            "Z" => 8,
            _ => 0

        },
        "B" => match my_hand {
            "X" => 1,
            "Y" => 5,
            "Z" => 9,
            _ => 0
        },
        "C" => match my_hand {
            "X" => 2,
            "Y" => 6,
            "Z" => 7,
            _ => 0
        },
        _ => 0
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let v:Vec<&str> = contents.split("\n").collect();

    let mut score: u32 = 0;

    for hands_played in v.into_iter() {
        let hands: Vec<&str> = hands_played.split(' ').collect();
        // println!("{:?}\n", hands);
        score += play_game2(hands[0], hands[1])
    }

    println!("Final score is {}", score);
}
