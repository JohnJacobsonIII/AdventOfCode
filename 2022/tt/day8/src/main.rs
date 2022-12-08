use std::env;
use std::fs;

fn left_distance(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> u32 {
    let mut distance: u32 = 0;
    for j in (0..n).rev() {
        if grid[m][j] >= grid[m][n] {
            distance+=1;
            break;
        }
        distance+=1;
    }
    // println!("{distance}");
    distance
}

fn right_distance(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> u32 {
    let mut distance: u32 = 0;
    for j in n+1..grid[0].len() {
        if grid[m][j] >= grid[m][n] {
            distance+=1;
            break;
        }
        distance+=1;
    }
    // println!("{distance}");
    distance
}

fn top_distance(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> u32 {
    let mut distance: u32 = 0;
    for i in (0..m).rev() {
        if grid[i][n] >= grid[m][n] {
            distance+=1;
            break;
        }
        distance+=1;
    }
    // println!("{distance}");
    distance
}

fn bottom_distance(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> u32 {
    let mut distance: u32 = 0;
    for i in m+1..grid.len() {
        if grid[i][n] >= grid[m][n] {
            distance+=1;
            break;
        }
        distance+=1;
    }
    // println!("{distance}");
    distance
}

fn compute_distance_grid(grid: Vec<Vec<u32>>) -> Vec<Vec<u32>> {
    let mut distance_grid: Vec<Vec<u32>> = Vec::new();

    for i in 0..grid.len() {
    // for i in 0..2 {
        distance_grid.push(Vec::new());
        for j in 0..grid[0].len() {
        // for j in 0..2 {
            distance_grid[i].push(left_distance(&grid, i, j) *
                right_distance(&grid, i, j) *
                bottom_distance(&grid, i, j) *
                top_distance(&grid, i, j));
            // println!();
        }
    }

    distance_grid
}

fn check_left(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> bool {
    for j in 0..n {
        if grid[m][j] >= grid[m][n] {
            return false
        }
    }
    true
}

fn check_right(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> bool {
    for j in n+1..grid[0].len() {
        if grid[m][j] >= grid[m][n] {
            return false
        }
    }
    true
}

fn check_top(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> bool {
    for i in 0..m {
        if grid[i][n] >= grid[m][n] {
            return false
        }
    }
    true
}

fn check_bottom(grid: &Vec<Vec<u32>>, m: usize, n: usize) -> bool {
    for i in m+1..grid.len() {
        if grid[i][n] >= grid[m][n] {
            return false
        }
    }
    true
}

fn count_visible_trees(grid: Vec<Vec<u32>>) -> u32 {
    let mut count: u32 = 0;

    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            if check_left(&grid, i, j) ||
                check_right(&grid, i, j) ||
                check_bottom(&grid, i, j) ||
                check_top(&grid, i, j) {
                // print!("T");
                count += 1;
            } else {
                // print!("F");
            }
        }
        // println!();
    }

    // count += 2*(grid[0].len() as u32 + grid.len() as u32);

    count
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let contents_split:Vec<&str> = contents.split("\n").collect();
    // println!("{:?}", contents_split);

    let char_grid: Vec<Vec<char>> = contents_split.iter().
        map(|x| x.chars().collect()).collect();

    let int_grid: Vec<Vec<u32>> = char_grid.iter().
        map(|x| x.iter().
            map(|y| (*y).to_digit(10).unwrap()).collect()).collect();

    let distance_grid: Vec<Vec<u32>> = compute_distance_grid(int_grid);

    // println!("{:?}", int_grid);

    // println!("{}", count_visible_trees(int_grid));
    // println!("{:?}", distance_grid);

    let max_value_vector: Vec<u32> = distance_grid.iter().map(|x| match x.iter().max() {
        Some(max) => *max,
        None => 0,
    }).collect();

    let max_value = match max_value_vector.iter().max() {
        Some(max) => *max,
        None => 0,
    };

    println!("{}", max_value);
}