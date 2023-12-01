

fn parse_commands(text: &str) -> Vec<(&str, &str, &str)> {
    let mut commands = vec![];

    for chunk in text.split('$') {
        let mut chunk_splitter = chunk.splitn(2, '\n');
        let full_command = chunk_splitter.next().unwrap();

        let mut command_splitter = full_command.split_whitespace();
        let command = command_splitter.next().unwrap();

        let (arg, output) = match command {
            "ls" => ("", chunk_splitter.next().unwrap()),
            "cd" => (command_splitter.next().unwrap(), ""),
            _ => panic!("Invalid command: '{}'", command),
        };

        commands.push((command, arg, output))
    }

    commands
}

// let mut contents = cwd_path[cwd_path.len() - 1].contents;
// for line in output.lines() {
//     let mut splitter = line.split_whitespace();
//     let first = splitter.next().unwrap();
//     let name = splitter.next().unwrap();
//     assert!(splitter.next().is_none());
//     if first == "dir" {
//         let next_co = & Vec::
//         let next_bn = branch_node{ size: None, contents: next_co };
//         let next_in = inner{ branch: next_bn};
//         let next_fi = file{ name: name.to_string(), content: next_in };
//         contents.push(next_fi);
//     } else {
// todo!();
//     }

// fn construct_filesystem(commands: Vec<(&str, &str, &str)>) {
//     let root = todo!();
//     let mut cwd_path = vec![];
//     for (command, arg, output) in commands {
//         match (command, arg) {
//             ("cd", "/") => {
//                 cwd_path.clear();
//                 cwd_path.push(root);
//             }
//             ("cd", "..") => {
//                 cwd_path.pop();
//             }
//             ("cd", x) => {
//                 let cwd = cwd_path[cwd_path.len() - 1];
//                 let new_cwd = todo!();
//                 cwd_path.push(new_cwd);
//             }
//             ("ls", "") => {
//                 todo!();
//             }
//             _ => panic!("Invalid command: '{} {}'", command, arg),
//         }
//     }
//     todo!()
// }

fn solve_part1(text: &str) -> usize {
    let commands = parse_commands(text);
    0
}

pub fn solve_day7(text: &str) -> (usize, usize) {
    let part1 = solve_part1(text);

    (part1, 0)
}
