use std::collections::HashSet;
use std::env;
use std::fs;

struct State {
    head: [i32; 2],
    tail: [i32; 2],
    tail_visited: HashSet<[i32; 2]>,
    motions: Vec<Vec<String>>,
}

impl State {
    fn is_adjacent(&self) -> bool {
        (self.head[0]-1 == self.tail[0] && self.head[1]-1 == self.tail[1]) ||
            (self.head[0] == self.tail[0] && self.head[1]-1 == self.tail[1]) ||
            (self.head[0]+1 == self.tail[0] && self.head[1]-1 == self.tail[1]) ||
            (self.head[0]-1 == self.tail[0] && self.head[1] == self.tail[1]) ||
            (self.head[0] == self.tail[0] && self.head[1] == self.tail[1]) ||
            (self.head[0]+1 == self.tail[0] && self.head[1] == self.tail[1]) ||
            (self.head[0]-1 == self.tail[0] && self.head[1]+1 == self.tail[1]) ||
            (self.head[0] == self.tail[0] && self.head[1]+1 == self.tail[1]) ||
            (self.head[0]+1 == self.tail[0] && self.head[1]+1 == self.tail[1])
    }

    fn run_motions(mut self) {
        for motion in self.motions.iter() {
            self.move_knots(&motion[0], motion[1].parse().unwrap());
        }


    }

    fn move_knots(mut self, direction: &String,  steps: u32) {
        assert!(self.is_adjacent());

        for _ in 0..steps {
            self.move_a_step(direction);
        }
    }

    fn move_a_step(mut self, direction: &String) {
        assert!(self.is_adjacent());

        match &direction[..] {
            "L" => {
                self.head[0]-1;

                if !self.is_adjacent() {
                    self.tail = [self.head[0] + 1, self.head[1]];
                }
            }
            "R" => {
                self.head[0]+1;

                if !self.is_adjacent() {
                    self.tail = [self.head[0] - 1, self.head[1]];
                }
            }
            "D" => {
                self.head[1]-1;

                if !self.is_adjacent() {
                    self.tail = [self.head[0], self.head[1]+1];
                }
            }
            "U" => {
                self.head[1]+1;

                if !self.is_adjacent() {
                    self.tail = [self.head[0], self.head[1]-1];
                }
            }
            _ => {
                println!("Invalid direction!");
            }
        }

        self.tail_visited.insert(self.tail);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let contents_split: Vec<&str> = contents.split('\n').rev().collect();
    println!("{:?}", contents_split);

    let mut state: State = State {
        head: [0, 0],
        tail: [0, 0],
        tail_visited: HashSet::new(),
        motions: contents_split.iter().
            map(|x| x.split(' ').
                map(|y| String::from(y)).collect()).rev().collect(),
    };

    state.run_motions();

    println!("{}", state.tail_visited.len());
}
