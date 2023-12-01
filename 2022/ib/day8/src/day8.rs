

fn parse_char_to_u8(c: char) -> u8 {
    assert!(c.is_numeric());
    let code = c as u8;
    code - b'0'
}

fn parse_tree_grid(text: &str) -> Vec<Vec<u8>> {
    text.lines()
        .map(|line| line.chars().map(parse_char_to_u8).collect())
        .collect()
}

fn is_visible(grid: &Vec<Vec<u8>>, coord: (usize, usize)) -> bool {
    let (x, y) = coord;
    let target_height = grid[x][y];
    let grid_x = grid.len();
    let grid_y = grid[0].len();

    x == 0
        || x == grid_x - 1
        || y == 0
        || y == grid_y - 1
        || (0..x).map(|sx| grid[sx][y]).max().unwrap() < target_height
        || ((x + 1)..grid_x).map(|sx| grid[sx][y]).max().unwrap() < target_height
        || (0..y).map(|sy| grid[x][sy]).max().unwrap() < target_height
        || ((y + 1)..grid_y).map(|sy| grid[x][sy]).max().unwrap() < target_height
}

enum Direction {
    North,
    South,
    East,
    West,
}

fn move_in_direction(coord: (usize, usize), dir: &Direction) -> (usize, usize) {
    match dir {
        Direction::North => (coord.0, coord.1 + 1),
        Direction::South => (coord.0, coord.1 - 1),
        Direction::East => (coord.0 + 1, coord.1),
        Direction::West => (coord.0 - 1, coord.1),
    }
}

fn direction_scenic_score(
    grid: &Vec<Vec<u8>>,
   target_height: u8,
   coord: (usize, usize),
   dir: &Direction,
) -> usize {
    let (x, y) = coord;
    let grid_x = grid.len();
    let grid_y = grid[0].len();
    if x==0 || y==0 || x == grid_x - 1 || y == grid_y - 1 {
        return 0
    }
    let next_coord = move_in_direction(coord, dir);
    let (next_x, next_y) = next_coord;
    if grid[next_x][next_y] >= target_height {
        1
    } else {
        1 + direction_scenic_score(grid, target_height, next_coord, dir)
    }
}

fn scenic_score(grid: &Vec<Vec<u8>>, x: usize, y: usize) -> usize {
    let target_height = grid[x][y];

    let directions = vec![Direction::North, Direction::South, Direction::East, Direction::West];

    directions.iter().map(|dir| {direction_scenic_score(grid, target_height, (x,y), dir)}).product()
}

fn solve_part1(grid: &Vec<Vec<u8>>) -> usize {
    (0..grid.len())
        .map(|x| {
            (0..grid[0].len())
                .map(|y| is_visible(grid, (x, y)) as usize)
                .sum::<usize>()
        })
        .sum()
}

fn solve_part2(grid: &Vec<Vec<u8>>) -> usize {
    (0..grid.len())
        .map(|x| {
            (0..grid[0].len())
                .map(|y| scenic_score(grid, x, y))
                .max()
                .unwrap()
        })
        .max()
        .unwrap()
}

pub fn solve_day8(text: &str) -> (usize, usize) {
    let grid = parse_tree_grid(text);
    let part1 = solve_part1(&grid);
    let part2 = solve_part2(&grid);
    (part1, part2)
}
