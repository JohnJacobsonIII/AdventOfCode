use std::env;
use std::fs;

fn letter_height(letter: char) -> u32 {
    match letter {
        'S' => 1,
        'E' => 26,
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
        _ => 0,
    }
}

fn grid_printer(elevation_grid: Vec<Vec<(u32, u32)>>) {
    for row in elevation_grid {
        for height in row {
            print!("{:>2},", height.0);
        }
        println!();
    }
}

fn find_shortest_path(elevation_grid: Vec<Vec<(u32, u32)>>, start: (u32, u32), end: (u32, u32)) {
    elevation_grid[start.0][start.1].1 = 0;


}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let contents_split: Vec<&str> = contents.split("\n").collect();

    let mut elevation_grid_letters: Vec<Vec<char>> = Vec::new();

    for row in contents_split {
        elevation_grid_letters.push(row.chars().collect());
    }

    let mut start: (u32, u32) = (0, 0);
    let mut end: (u32, u32) = (0, 0);

    for i in 0..elevation_grid_letters.len() {
        for j in 0..elevation_grid_letters[i].len() {
            if elevation_grid_letters[i][j] == 'S' {
                start = (i as u32, j as u32);
            } else if elevation_grid_letters[i][j] == 'E' {
                end = (i as u32, j as u32);
            }
        }
    }

    println!("{:?}", start);
    println!("{:?}", end);

    let elevation_grid: Vec<Vec<(u32, u32)>> =
        elevation_grid_letters.into_iter().
            map(|x| x.into_iter().
                map(|y| (letter_height(y), u32::MAX)).
                collect()).
            collect();

    grid_printer(elevation_grid);
//    println!("{:?}", elevation_grid);
}
