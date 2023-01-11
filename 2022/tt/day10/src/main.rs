use std::env;
use std::fs;

#[derive(Debug)]
struct State {
    cycles: i32,
    fetch_next: bool,
    x: i32,
    current: String,
    parameter: i32,
    incubation_cycles: u32,
    commands: Vec<String>,

}

impl State {
    fn increment_cycle(&mut self) {
        if self.fetch_next == true {
            self.execute_current_fetch_next_command();
        }

        self.cycles += 1;
        self.incubation_cycles += 1;

        if self.current == "addx" &&
            self.incubation_cycles == 2 {
            self.fetch_next = true;
        }
    }

    fn execute_current_fetch_next_command(&mut self) {
        if self.current == "addx" {
            self.x += &self.parameter;
        }

        if let Some(command) = self.commands.pop() {
            let command_split: Vec<&str> = command.split(' ').collect();
            self.current = command_split[0].to_string();
            if command_split.len() == 2 {
                self.fetch_next = false;
                self.parameter = command_split[1].parse::<i32>().unwrap();
            };
            self.incubation_cycles = 0;
        } else {
            self.parameter = 0
        };

    }

    fn execute_till_n_cycles(&mut self, n: i32) {
        while self.cycles < n {
            self.increment_cycle();
            self.draw_pixel();
//            println!("{:?}", self);

        }
    }

    fn calculate_signal_strength(&self) -> i32 {
        &self.x * &self.cycles
    }

    fn draw_pixel(&self) {
        if (&self.cycles - 1)%40 == &self.x - 1 ||
            (&self.cycles - 1)%40 == self.x ||
            (&self.cycles - 1)%40 == &self.x + 1 {
            print!("#");
        } else {
            print!(".");
        }

        if &self.cycles % 40 == 0 {
            println!();
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let contents_split: Vec<&str> = contents.split('\n').rev().collect();
//    println!("{:?}", contents_split);

    let mut cpu_state = State {
        cycles: 0,
        fetch_next: true,
        x: 1,
        current: "noop".to_string(),
        parameter: 0,
        incubation_cycles: 0,
        commands: contents_split.iter().map(|x| String::from(*x)).collect(),
    };

//    let mut signal_strength: i32 = 0;

//    println!("{:?}", cpu_state);
    cpu_state.execute_till_n_cycles(240);
//    println!("{:?}", cpu_state);

//    cpu_state.execute_till_n_cycles(20);
//    signal_strength += cpu_state.calculate_signal_strength();
//    println!("{:?}", cpu_state);
//    cpu_state.execute_till_n_cycles(60);
//    signal_strength += cpu_state.calculate_signal_strength();
//    println!("{:?}", cpu_state);
//    cpu_state.execute_till_n_cycles(100);
//    signal_strength += cpu_state.calculate_signal_strength();
//    println!("{:?}", cpu_state);
//    cpu_state.execute_till_n_cycles(140);
//    signal_strength += cpu_state.calculate_signal_strength();
//    println!("{:?}", cpu_state);
//    cpu_state.execute_till_n_cycles(180);
//    signal_strength += cpu_state.calculate_signal_strength();
//    println!("{:?}", cpu_state);
//    cpu_state.execute_till_n_cycles(220);
//    signal_strength += cpu_state.calculate_signal_strength();
//    println!("{:?}", cpu_state);
//
//    println!("{}", signal_strength);

}
