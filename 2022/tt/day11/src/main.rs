use std::env;
use std::fs;
use std::collections::VecDeque;

#[derive(Debug)]
enum WorryModifier {
    Add,
    Multiply,
}

#[derive(Debug)]
struct Monkey {
    items: VecDeque<Vec<u32>>,
    worry_modifier: u32,
    worry_modifying_operation: WorryModifier,
    divisibility_test: u32,
    pass_to_on_true: u32,
    pass_to_on_false: u32,
    items_inspected: u32,
    held_item: Option<Vec<u32>>,
}

impl Monkey {
    fn inspect_item(&mut self, divisibility_tests: &Vec<u32> ,number_of_monkeys: usize) {
        assert!(!self.items.is_empty());

        // println!("{:?}", self);

        if let Some(item_inspected) = self.items.pop_front() {
            let operand: Vec<u32> = if self.worry_modifier == 0 { item_inspected.to_vec() } else { vec![self.worry_modifier; number_of_monkeys] };

            self.held_item = Some(match self.worry_modifying_operation {
                WorryModifier::Add =>  {
                    let mut z = vec![0; number_of_monkeys];
                    for i in 0..number_of_monkeys {
                        z[i] = (item_inspected[i] + operand[i])%divisibility_tests[i]
                    }
                    z
                },
                WorryModifier::Multiply => {
                    let mut z = vec![0; number_of_monkeys];
                    for i in 0..number_of_monkeys {
                        z[i] = (item_inspected[i] * operand[i])%divisibility_tests[i];
                    }
                    z
                },
            });

            self.items_inspected += 1;
        }

        // println!("{:?}", self);
        // println!();
    }

    fn perform_test(&mut self, i: usize) -> (Vec<u32>, u32) {
        assert!(self.held_item.is_some());

        let item:Vec<u32> = self.held_item.as_ref().unwrap().to_vec();
        return if item[i] == 0 {
            self.held_item = None;
            (item, self.pass_to_on_true)
        } else {
            self.held_item = None;
            (item, self.pass_to_on_false)
        }
    }

    fn pass_item(&mut self, item: Vec<u32>) {
        self.items.push_back(item);
    }

    fn mod_held_item(&mut self, i: usize, value: u32) {
        assert!(self.held_item.is_some());

        self.held_item.as_mut().unwrap()[i] = value;
    }
}

fn parse_input(mut monkeys: Vec<Monkey>, input: Vec<&str>) -> (Vec<u32>, Vec<Monkey>) {
    let mut divisibility_tests: Vec<u32> = Vec::new();

    for string_monkey in &input {

        let monkey_notes: Vec<&str> = string_monkey.split("\n").collect();

//        println!("{:?}", monkey_notes);

        let items_note: Vec<&str> = monkey_notes[1].split(' ').collect();

        let mut items: VecDeque<Vec<u32>> = VecDeque::new();
        for i in 4..items_note.len() {
            let item_string: Vec<&str> = items_note[i].split(',').collect();

            items.push_back(vec![item_string[0].parse::<u32>().unwrap(); input.len()]);
        }

        let operation_notes: Vec<&str> = monkey_notes[2].split(' ').collect();

        let mut operand: u32 = 0;
        if operation_notes[7].chars().all(char::is_numeric) {
            operand = operation_notes[7].parse::<u32>().unwrap();
        }

        let test_notes: Vec<&str> = monkey_notes[3].split(' ').collect();
        let pass_on_true_notes: Vec<&str> = monkey_notes[4].split(' ').collect();
        let pass_on_false_notes: Vec<&str> = monkey_notes[5].split(' ').collect();

        divisibility_tests.push(test_notes[5].parse::<u32>().unwrap());

        monkeys.push(Monkey {
            items,
            worry_modifier: operand,
            worry_modifying_operation: if operation_notes[6] == "*" { WorryModifier::Multiply } else { WorryModifier::Add },
            divisibility_test: test_notes[5].parse::<u32>().unwrap(),
            pass_to_on_true: pass_on_true_notes[9].parse::<u32>().unwrap(),
            pass_to_on_false: pass_on_false_notes[9].parse::<u32>().unwrap(),
            items_inspected: 0,
            held_item: None,
        });
    }

   for i in 0..monkeys.len() {
       // println!("{:?}", monkeys[i]);

       for j in 0..monkeys[i].items.len() {
           for k in 0..divisibility_tests.len() {
               monkeys[i].items[j][k] %= divisibility_tests[k];
           }
       }

       // println!("{:?}", monkeys[i]);
   }
    // println!();

    (divisibility_tests, monkeys)
}

fn simulate_round(mut monkeys: Vec<Monkey>, divisibility_tests: &Vec<u32>) -> Vec<Monkey> {
    let number_of_monkeys: usize = monkeys.len();

    for i in 0..number_of_monkeys {
        while !monkeys[i].items.is_empty() {
            monkeys[i].inspect_item(divisibility_tests, number_of_monkeys);

            let (item, monkey_index): (Vec<u32>, u32) = monkeys[i].perform_test(i);

            monkeys[monkey_index as usize].pass_item(item);
        }
    }

    monkeys
}

fn simulate_n_rounds(mut monkeys: Vec<Monkey>, divisibility_tests: Vec<u32> ,n: u32) -> Vec<Monkey> {
    for _ in 0..n {
        monkeys = simulate_round(monkeys, &divisibility_tests);
    }

    monkeys
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let string_monkeys: Vec<&str> = contents.split("\n\n").collect();
//    println!("{:?}", string_monkeys);

    let mut monkeys: Vec<Monkey> = Vec::new();
    let mut divisibility_tests: Vec<u32> = Vec::new();

    (divisibility_tests, monkeys) = parse_input(monkeys, string_monkeys);

   monkeys = simulate_n_rounds(monkeys, divisibility_tests, 10000);

    for monkey in monkeys {
        println!("{:?}", monkey);
    }

}
