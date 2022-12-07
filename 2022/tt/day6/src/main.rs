use std::env;
use std::fs;
use std::collections::HashSet;

fn find_marker(data_stream: String, packet_length: usize) -> usize {
    for i in 0..data_stream.len() {
        let mut packet = HashSet::new();
        for j in i..(i+packet_length) {
            packet.insert(data_stream.chars().collect::<Vec<char>>()[j as usize]);
        }
        if packet.len() == packet_length {
            return i+packet_length
        }
        // println!("{:?}", packet);
    }
    0 as usize
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    // println!("{}", contents);
    println!("{}",find_marker(contents, 14));

}
