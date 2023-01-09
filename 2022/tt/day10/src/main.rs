use std::env;
use std::fs;

#[derive(Debug)]
struct State {
    cycles: u32,
    x: i32,
    commands: Vec<String>
}

impl State {
    fn execute_next_command(&mut self) {
        let command = self.commands.pop().unwrap();
//        println!("{}", command)

        let command_split: Vec<&str> = command.split(' ').collect();
//        println!("{}", command_split.len());

        match command_split.len() {
            1 => {
                self.cycles+=1;
            },
            2 => {
                self.cycles+=2;
                self.x+=command_split[1].parse::<i32>().unwrap();
            },
            _ => {},
        }
    }

    fn execute_n_commands(&mut self, n: u32) {
        for _ in 0..n {
            self.execute_next_command();
        }
    }

    fn execute_till_n_cycles(&mut self, n: u32) {
        while self.cycles < n {
            let command = self.commands.last().unwrap();
//        println!("{}", command)

            let command_split: Vec<&str> = command.split(' ').collect();
//        println!("{}", command_split.len());

            match command_split.len() {
                2 => {
                    if &self.cycles+3 > n {
                        break;
                    };
                },
                _ => {},
            }
            self.execute_next_command();

        }
    }

    fn calculate_signal_strength(self) -> i32 {
        self.x * self.cycles as i32
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
        x: 1,
        commands: contents_split.iter().map(|x| String::from(*x)).collect(),
    };

    let mut signal_strength: i32 = 0;

//    println!("{:?}", cpu_state);
//    cpu_state.execute_n_commands(20);
//    println!("{:?}", cpu_state);

//    cpu_state.execute_till_n_cycles(20);
//    signal_strength += &cpu_state.x * 20;
//    cpu_state.execute_till_n_cycles(60);
//    signal_strength += &cpu_state.x * 60;
//    cpu_state.execute_till_n_cycles(100);
//    signal_strength += &cpu_state.x * 100;
//    cpu_state.execute_till_n_cycles(140);
//    signal_strength += &cpu_state.x * 140;
//    cpu_state.execute_till_n_cycles(180);
//    signal_strength += &cpu_state.x * 180;
//    cpu_state.execute_till_n_cycles(220);
//    signal_strength += &cpu_state.x * 220;



    println!("{}", signal_strength);

}
