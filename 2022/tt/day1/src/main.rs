use std::env;
use std::fs;

fn max_three_calories(v: Vec<&str>) -> u32 {
    let mut max1: u32 = 0;
    let mut max2: u32 = 0;
    let mut max3: u32 = 0;

    for elf_calories_string in v.into_iter() {
        let elf_calories: Vec<&str> = elf_calories_string.split('\n').collect();

        let elf_calorie_int: Vec<u32> = elf_calories.iter().map(|x| x.parse::<u32>().unwrap()).collect();

        let mut sum: u32 = 0;
        for calorie in elf_calorie_int.into_iter() {
            sum += calorie;
        }
        // let sum = elf_calorie_int.into_iter().reduce(|a, b| a + b);
//        let sum = elf_calorie_int.into_iter().fold(0, |a, b| a + b);
        if sum > max3 {
            if sum > max2 {
                if sum > max1 {
                    max3 = max2;
                    max2 = max1;
                    max1 = sum;
                } else {
                    max3 = max2;
                    max2 = sum;
                }
            } else {
                max3 = sum;
            }
        }
        // println!("{:?}", elf_calorie_int);
    }

    max1+max2+max3
}

fn max_calories(v: Vec<&str>) -> u32 {
    let mut max_sum: u32 = 0;

    for elf_calories_string in v.into_iter() {
        let elf_calories: Vec<&str> = elf_calories_string.split('\n').collect();

        let elf_calorie_int: Vec<u32> = elf_calories.iter().map(|x| x.parse::<u32>().unwrap()).collect();

        let mut sum: u32 = 0;
        for calorie in elf_calorie_int.into_iter() {
            sum += calorie;
        }
        // let sum = elf_calorie_int.into_iter().reduce(|a, b| a + b);
        if sum > max_sum {
            max_sum = sum;
        }
        // println!("{:?}", elf_calorie_int);
    }

    max_sum
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let v:Vec<&str> = contents.split("\n\n").collect();
    // println!("{:?}", v);

    let result: u32 = max_three_calories(v);
    println!("\n{result}\n");
    // println!("With text:\n{contents}");
}
