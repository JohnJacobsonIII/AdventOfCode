use std::env;
use std::fs;

struct SupplyStacks {
    stacks: Vec<Vec<char>>,
    rearrangement_procedure: Vec<(u32, u32, u32)>,
}

impl SupplyStacks {
    fn move_crates(&mut self) {
        let (number_of_crates, source_stack, destination_stack) = self.rearrangement_procedure.pop().unwrap();

        for _ in 0..number_of_crates {
            let value_to_push = self.stacks[(source_stack-1) as usize].pop().unwrap();
            self.stacks[(destination_stack-1) as usize].push(value_to_push);
        }
//        println!("{:?}", self.stacks);
    }

    fn move_crates_in_stacks(&mut self) {
        let (number_of_crates, source_stack, destination_stack) = self.rearrangement_procedure.pop().unwrap();

        let mut values_to_push: Vec<char> = Vec::new();
        for _ in 0..number_of_crates {
            values_to_push.push(self.stacks[(source_stack-1) as usize].pop().unwrap());
        }
        for _ in 0..number_of_crates {
            self.stacks[(destination_stack-1) as usize].push(values_to_push.pop().unwrap());
        }

//        println!("{:?}", self.stacks);
    }

    fn complete_rearrangement_procedure(&mut self) {
        while !self.rearrangement_procedure.is_empty() {
            self.move_crates_in_stacks();
        }
    }

    fn get_stack_top(&self) -> Vec<char>{
        let stack_tops: Vec<char> = self.stacks.iter().map(|x| *x.last().unwrap()).collect();

//        for stack in self.stacks.iter() {
//            stack_tops.push(*stack.last().unwrap())
//        }

        stack_tops
    }
}

fn read_stacks(data: &str) -> Vec<Vec<char>>{
    let mut stacks: Vec<Vec<char>> = Vec::new();
    for (i, line) in data.split('\n').rev().enumerate() {
        if i == 0 {
            for (j, _) in line.chars().enumerate() {
                if j%4 == 1 {
                    stacks.push(Vec::new());
                }
            }
        } else {
            for (j, character) in line.chars().enumerate() {
                if j%4 == 1 && character != ' ' {
                    stacks[j/4].push(character);
//                    println("{} character is {}", j/4, character)
                }
            }
        }
    }

    stacks
}

fn read_rearrangement_procedure(data: &str) -> Vec<(u32, u32, u32)>{
    let mut rearrangement_procedure: Vec<(u32, u32, u32)> = Vec::new();
    for line in data.split('\n').rev() {
        let line_split: Vec<&str> = line.split(' ').collect();
        rearrangement_procedure.push((line_split[1].parse().unwrap(),
                                      line_split[3].parse().unwrap(),
                                      line_split[5].parse().unwrap()))
    }

    rearrangement_procedure
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let contents_split:Vec<&str> = contents.split("\n\n").collect();

    let mut _supply_stacks = SupplyStacks {
        stacks: read_stacks(contents_split[0]),
        rearrangement_procedure: read_rearrangement_procedure(contents_split[1]),
    };

    _supply_stacks.complete_rearrangement_procedure();
    println!("{:?}", _supply_stacks.get_stack_top());
}
