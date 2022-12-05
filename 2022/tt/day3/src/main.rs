use std::env;
use std::fs;

fn find_badge_item_type(rucksack0: &str, rucksack1: &str, rucksack2: &str) -> char {
    let mut error_item_type = '0';
    for item_type1 in rucksack0.chars() {
        for item_type2 in rucksack1.chars() {
            for item_type3 in rucksack2.chars() {
                if item_type1 == item_type2 && item_type2 == item_type3 {
                    error_item_type = item_type1;
                    break;
                }
            }
        }
    }
    error_item_type
}

fn find_error_item_type(rucksack: (&str, &str)) -> char {
    let mut error_item_type = '0';
    for item_type1 in rucksack.0.chars() {
        for item_type2 in rucksack.1.chars() {
            if item_type1 == item_type2 {
                error_item_type = item_type1;
                break;
            }
        }
    }
    error_item_type
}

fn get_type_priority(item_type: char) -> u32 {
    match item_type {
        'a' => 1,
        'b' => 2,
        'c' => 3,
        'd' => 4,
        'e' => 5,
        'f' => 6,
        'g' => 7,
        'h' => 8,
        'i' => 9,
        'j' => 10,
        'k' => 11,
        'l' => 12,
        'm' => 13,
        'n' => 14,
        'o' => 15,
        'p' => 16,
        'q' => 17,
        'r' => 18,
        's' => 19,
        't' => 20,
        'u' => 21,
        'v' => 22,
        'w' => 23,
        'x' => 24,
        'y' => 25,
        'z' => 26,
        'A' => 27,
        'B' => 28,
        'C' => 29,
        'D' => 30,
        'E' => 31,
        'F' => 32,
        'G' => 33,
        'H' => 34,
        'I' => 35,
        'J' => 36,
        'K' => 37,
        'L' => 38,
        'M' => 39,
        'N' => 40,
        'O' => 41,
        'P' => 42,
        'Q' => 43,
        'R' => 44,
        'S' => 45,
        'T' => 46,
        'U' => 47,
        'V' => 48,
        'W' => 49,
        'X' => 50,
        'Y' => 51,
        'Z' => 52,
        _ => 0,
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let rucksacks: Vec<&str> = contents.split("\n").collect();

//    let rucksacks_with_compartments: Vec<(&str, &str)> = rucksacks.iter().
//        map(|x| x.split_at(x.chars().count()/2)).
//        collect();
//    let error_items_priorities: Vec<u32> = rucksacks_with_compartments.iter().
//        map(|x| get_type_priority(find_error_item_type(*x))).
//        collect();

    let mut error_items_priorities = Vec::new();
    for i in 0..(rucksacks.len()/3) {
        error_items_priorities.push(get_type_priority(find_badge_item_type(rucksacks[3*i],
                                                                           rucksacks[3*i+1],
                                                                           rucksacks[3*i+2])));
    }

    let priority_sum = error_items_priorities.into_iter().fold(0, |a, b| a + b);

    println!("Result: {}", priority_sum);
}
