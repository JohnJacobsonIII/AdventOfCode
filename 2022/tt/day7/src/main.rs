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

    fn ls(&self, directory: &str) {

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
        commands: contents_split.iter().map(|x| String::from(x)).collect(),
    };
}
