use std::env;
use std::fs;

struct State {
    pwd: Directory,
    commands: Vec<String>,
}

impl State {
    fn cd(mut self, directory: &str) {
        match directory {
            ".." => self.pwd = *self.pwd.parent.expect("{self.pwd.name} does not have a parent!"),
            "/" => while self.pwd.parent != None {
                self.pwd = *self.pwd.parent.expect("{self.pwd.name} does not have a parent!");
            },
            _ => {
                for dir in self.pwd.directories {
                    if dir.name == directory {
                        self.pwd = dir
                    }
                }
            },
        }
    }

    fn ls(&mut self) {
        let command: &String = match self.commands.last() {
            Some(command) => command,
            None => ""
        };

        while command.split(' ').collect()[0] != "$" {
            self.commands.pop();

            if command.split(' ').collect()[0] == "dir" {
                self.pwd.directories.push(Directory {
                    name: String::from(command.split(' ').collect()[1]),
                    parent: Some(Box::new(*self.pwd)),
                    ..Default::default()
                })
            } else {
                self.pwd.files.push(File {
                    name: String::from(command.split(' ').collect()[1]),
                    size: command.split(' ').collect()[1].parse().unwrap(),
                })
            }

            let command: &String = match self.commands.last() {
                Some(command) => command,
                None => ""
            };
        }
    }

    fn consume_command(&mut self) {
        while !self.commands.is_empty() {
            match self.commands.last() {
                Some(command) => {
                    match command.split(' ').collect()[0] {
                        "cd" => self.cd(command.split(' ').collect()[1]),
                        "ls" => self.ls(),
                        _ => ()
                    };
                }
                None => ()
            };
        }
    }
}

#[derive(PartialEq)] struct File {
    name: String,
    size: u32,
}

impl Default for File {
    fn default() -> Self {
        File {
            name: String::from("NoFile"),
            size: 0,
        }
    }
}

#[derive(PartialEq)] struct Directory {
    name: String,
    size: u32,
    parent: Option<Box<Directory>>,
    directories: Vec<Directory>,
    files: Vec<File>,
}

impl Default for Directory {
    fn default() -> Self {
        Directory {
            name: String::from("NoDirectory"),
            size: 0,
            parent: None,
            directories: Vec::new(),
            files: Vec::new(),
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
    println!("{:?}", contents_split);

    let state = State {
        pwd: Directory {
            name: String::from("/"),
            ..Default::default()
        },
        commands: contents_split.iter().map(|x| String::from(*x)).collect(),
    };



}
