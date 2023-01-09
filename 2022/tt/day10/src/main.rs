use std::env;
use std::fs;

struct State {
    cycles: u32,
    x: i32,
    commands: Vec<String>
}

impl State {
    fn execute_next_command(mut self) {
        let command = self.commands.pop().unwrap();
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



}
