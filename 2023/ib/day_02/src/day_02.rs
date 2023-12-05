use std::collections::HashMap;

use log::{debug, info, trace};

fn read_line(line: &str) -> (usize, Vec<(usize, usize, usize)>) {
    let (game_index_str, rest_str) = line.split_once(":").unwrap();
    assert!(game_index_str.len() > 5);
    assert!(game_index_str.starts_with("Game "));

    let game_index = &game_index_str[5..].parse::<usize>().unwrap();

    let mut cubes = vec![];
    for game_str in rest_str.split(";") {
        let (mut red, mut green, mut blue) = (0, 0, 0);
        for color_str in game_str.split(",") {
            let color_str = color_str.trim();
            let (number_str, color) = color_str.split_once(" ").unwrap();
            let number = number_str.parse::<usize>().unwrap();
            match color {
                "red" => red = number,
                "green" => green = number,
                "blue" => blue = number,
                _ => panic!("Color string was '{}'", color),
            }
        }
        cubes.push((red, green, blue));
    }

    (*game_index, cubes)
}

fn parse_lines(text: &str) -> HashMap<usize, Vec<(usize, usize, usize)>> {
    let mut games = HashMap::new();

    for line in text.lines() {
        if line.trim().len() == 0 {
            continue;
        }
        let (game, sets) = read_line(line);
        games.insert(game, sets);
    }

    games
}

fn do_part_1(text: &str) -> usize {
    let games = parse_lines(text);
    let (max_red, max_green, max_blue) = (12, 13, 14);
    let mut possible_sum = 0;

    for (game, sets) in games {
        let mut is_game_possible = true;
        for (red, green, blue) in sets {
            if red > max_red || green > max_green || blue > max_blue {
                is_game_possible = false;
                break;
            }
        }
        if is_game_possible {
            possible_sum += game;
        }
    }

    possible_sum
}

fn do_part_2(text: &str) -> usize {
    let games = parse_lines(text);
    let mut power_sum = 0;

    for (game, sets) in games {
        let (mut min_red, mut min_green, mut min_blue) = (0, 0, 0);
        for (red, green, blue) in sets {
            if red > min_red {
                min_red = red;
            }
            if green > min_green {
                min_green = green;
            }
            if blue > min_blue {
                min_blue = blue;
            }
        }

        let power = min_red * min_green * min_blue;
        power_sum += power;
    }

    power_sum
}

/**
 * Answers day 2 of advent of code 2023, if possible.
 **/
pub fn solve_day_02(text: &str) -> (usize, usize) {
    let part1 = do_part_1(text);
    let part2 = do_part_2(text);

    // Reference answers:
    // Part 1: 54338
    // Part 2: 53389
    (part1, part2)
}
