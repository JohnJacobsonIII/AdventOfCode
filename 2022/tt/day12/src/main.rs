use std::env;
use std::fs;
use colored::Colorize;

const MAX_STEPS: u32 = 999;

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

fn grid_printer(elevation_grid: Vec<Vec<(u32, u32, char)>>, index: u32) {
    if index == 0 {
        for row in elevation_grid {
            for height in row {
                if height.2 != '.' {
                    print!("{:>3},", height.0.to_string().green());
                } else {
                    print!("{:>3},", height.0.to_string().red());
                }
            }
            println!();
        }
    } else if index == 1 {
        for row in elevation_grid {
            for height in row {
                if height.2 != '.' {
                    print!("{:>3},", height.1.to_string().green());
                } else {
                    print!("{:>3},", height.1.to_string().red());
                }
            }
            println!();
        }
    } else if index == 2 {
        for row in elevation_grid {
            for height in row {
                if height.2 != '.' {
                    print!("{:>3},", height.2.to_string().green());
                } else {
                    print!("{:>3},", height.2.to_string().red());
                }
            }
            println!();
        }
    }
}

// Colors the path from start to end in the elevation grid.
fn color_shortest_path(mut elevation_grid: Vec<Vec<(u32, u32, char)>>, start: (u32, u32, u32), end: (u32, u32)) -> Vec<Vec<(u32, u32, char)>> {
    let mut current_coordinate = end;
    elevation_grid[end.0 as usize][end.1 as usize].2 = 'E';

    while current_coordinate != (start.0, start.1) {
        if current_coordinate.1 > 0 && elevation_grid[current_coordinate.0 as usize][(current_coordinate.1 - 1) as usize].1 + 1 == elevation_grid[current_coordinate.0 as usize][current_coordinate.1 as usize].1 {
            elevation_grid[current_coordinate.0 as usize][(current_coordinate.1 - 1) as usize].2 = '>';
            current_coordinate.1 -= 1;
        } else if current_coordinate.0 > 0 && elevation_grid[(current_coordinate.0 - 1) as usize][current_coordinate.1 as usize].1 + 1 == elevation_grid[current_coordinate.0 as usize][current_coordinate.1 as usize].1 {
            elevation_grid[(current_coordinate.0 - 1) as usize][current_coordinate.1 as usize].2 = 'v';
            current_coordinate.0 -= 1;
        } else if current_coordinate.1 < elevation_grid[current_coordinate.0 as usize].len() as u32 - 1 && elevation_grid[current_coordinate.0 as usize][(current_coordinate.1 + 1) as usize].1 + 1 == elevation_grid[current_coordinate.0 as usize][current_coordinate.1 as usize].1 {
            elevation_grid[current_coordinate.0 as usize][(current_coordinate.1 + 1) as usize].2 = '<';
            current_coordinate.1 += 1;
        } else if current_coordinate.0 < elevation_grid.len() as u32 - 1 && elevation_grid[(current_coordinate.0 + 1) as usize][current_coordinate.1 as usize].1 + 1 == elevation_grid[current_coordinate.0 as usize][current_coordinate.1 as usize].1 {
            elevation_grid[(current_coordinate.0 + 1) as usize][current_coordinate.1 as usize].2 = '^';
            current_coordinate.0 += 1;
        } else {
            println!("Error: Could not find path. Examine: {}, {}", current_coordinate.0, current_coordinate.1);
            break;
        }
    }

    elevation_grid[start.0 as usize][start.1 as usize].2 = 'S';

    elevation_grid
}

fn get_all_start_points(elevation_grid: Vec<Vec<(u32, u32, char)>>) -> (Vec<Vec<(u32, u32, char)>>, Vec<(u32, u32, u32)>) {
    let mut start_points: Vec<(u32, u32, u32)> = Vec::new();

    for (row_index, row) in elevation_grid.iter().enumerate() {
        for (column_index, column) in row.iter().enumerate() {
            if column.0 == 1 {
                start_points.push((row_index as u32, column_index as u32, MAX_STEPS));
            }
        }
    }

    (elevation_grid, start_points)
}

// Finds the shortest path from start to end in the elevation grid.
fn find_shortest_path(mut elevation_grid: Vec<Vec<(u32, u32, char)>>, mut start: (u32, u32, u32), end: (u32, u32)) -> (Vec<Vec<(u32, u32, char)>>, (u32, u32, u32)) {
    // Reset elevation grid.
    for row in elevation_grid.iter_mut() {
        for height in row.iter_mut() {
            height.1 = MAX_STEPS;
            height.2 = '.';
        }
    }

    elevation_grid[start.0 as usize][start.1 as usize].1 = 0;
    let mut coorddinates_to_check: Vec<(u32, u32)> = Vec::new();

    coorddinates_to_check.push((start.0, start.1));

    while coorddinates_to_check.len() > 0 {
        let current_coordinate = coorddinates_to_check.pop().unwrap();
        let current_height = elevation_grid[current_coordinate.0 as usize][current_coordinate.1 as usize].0;
        let current_distance = elevation_grid[current_coordinate.0 as usize][current_coordinate.1 as usize].1;

        // Check the coordinate to the right.
        if current_coordinate.1 < elevation_grid[current_coordinate.0 as usize].len() as u32 - 1 {
            let right_coordinate = (current_coordinate.0, current_coordinate.1 + 1);
            let right_height = elevation_grid[right_coordinate.0 as usize][right_coordinate.1 as usize].0;
            let right_distance = elevation_grid[right_coordinate.0 as usize][right_coordinate.1 as usize].1;

            if right_height <= current_height+1 {
                let new_distance = current_distance + 1;
                if new_distance < right_distance {
                    elevation_grid[right_coordinate.0 as usize][right_coordinate.1 as usize].1 = new_distance;
                    coorddinates_to_check.push(right_coordinate);
                }
            }
        }

        // Check the coordinate to the left.
        if current_coordinate.1 > 0 {
            let left_coordinate = (current_coordinate.0, current_coordinate.1 - 1);
            let left_height = elevation_grid[left_coordinate.0 as usize][left_coordinate.1 as usize].0;
            let left_distance = elevation_grid[left_coordinate.0 as usize][left_coordinate.1 as usize].1;

            if left_height <= current_height+1 {
                let new_distance = current_distance + 1;
                if new_distance < left_distance {
                    elevation_grid[left_coordinate.0 as usize][left_coordinate.1 as usize].1 = new_distance;
                    coorddinates_to_check.push(left_coordinate);
                }
            }
        }

        // Check the coordinate above.
        if current_coordinate.0 > 0 {
            let above_coordinate = (current_coordinate.0 - 1, current_coordinate.1);
            let above_height = elevation_grid[above_coordinate.0 as usize][above_coordinate.1 as usize].0;
            let above_distance = elevation_grid[above_coordinate.0 as usize][above_coordinate.1 as usize].1;

            if above_height <= current_height+1 {
                let new_distance = current_distance + 1;
                if new_distance < above_distance {
                    elevation_grid[above_coordinate.0 as usize][above_coordinate.1 as usize].1 = new_distance;
                    coorddinates_to_check.push(above_coordinate);
                }
            }
        }

        // Check the coordinate below.
        if current_coordinate.0 < elevation_grid.len() as u32 - 1 {
            let below_coordinate = (current_coordinate.0 + 1, current_coordinate.1);
            let below_height = elevation_grid[below_coordinate.0 as usize][below_coordinate.1 as usize].0;
            let below_distance = elevation_grid[below_coordinate.0 as usize][below_coordinate.1 as usize].1;

            if below_height <= current_height+1 {
                let new_distance = current_distance + 1;
                if new_distance < below_distance {
                    elevation_grid[below_coordinate.0 as usize][below_coordinate.1 as usize].1 = new_distance;
                    coorddinates_to_check.push(below_coordinate);
                }
            }
        }
    }

    println!("The shortest path from ({}, {}) to ({}, {}) is {}.", start.0, start.1, end.0, end.1, elevation_grid[end.0 as usize][end.1 as usize].1);
    start.2 = elevation_grid[end.0 as usize][end.1 as usize].1;

    (elevation_grid, start)
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

    // let mut start: (u32, u32, u32) = (0, 0, MAX_STEPS);
    let mut end: (u32, u32) = (0, 0);

    for i in 0..elevation_grid_letters.len() {
        for j in 0..elevation_grid_letters[i].len() {
            // if elevation_grid_letters[i][j] == 'S' {
            //     start = (i as u32, j as u32, MAX_STEPS);
            // } else
            if elevation_grid_letters[i][j] == 'E' {
                end = (i as u32, j as u32);
            }
        }
    }

    let mut elevation_grid: Vec<Vec<(u32, u32, char)>> =
        elevation_grid_letters.into_iter().
            map(|x| x.into_iter().
                map(|y| (letter_height(y), MAX_STEPS, '.')).
                collect()).
            collect();

    let mut start_points: Vec<(u32, u32, u32)> = vec![];

    (elevation_grid, start_points) = get_all_start_points(elevation_grid);

    // println!("Start : {:?}", start);
    println!("Start points: {:?}", start_points);
    println!();
    println!("End   : {:?}", end);

    for i in 0..start_points.len() {
        (elevation_grid, start_points[i]) = find_shortest_path(elevation_grid, start_points[i], end);
    }

    // (elevation_grid, start) = find_shortest_path(elevation_grid, start, end);
    // elevation_grid = color_shortest_path(elevation_grid, start, end);

    start_points.sort_by(|a, b| a.2.cmp(&b.2));

    // println!("Start points: {:?}", start_points);

    (elevation_grid, start_points[0]) = find_shortest_path(elevation_grid, start_points[0], end);
    elevation_grid = color_shortest_path(elevation_grid, start_points[0], end);

    println!();

    grid_printer(elevation_grid, 1);
//    println!("{:?}", elevation_grid);
}
