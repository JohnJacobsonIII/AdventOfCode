use std::env;
use std::fs;

struct ElfPair {
    pair: (Section, Section),
}

impl ElfPair {
    fn containing_section_assignments(&self) -> bool {
        self.pair.0.can_hold(&self.pair.1)
    }

    fn overlapping_section_assignments(&self) -> bool {
        self.pair.0.overlapping(&self.pair.1)
    }
}

struct Section {
    section_start: u32,
    section_end: u32,
}

impl Section {
    fn can_hold(&self, other: &Section) -> bool {
        (self.section_start <= other.section_start && self.section_end >= other.section_end) ||
            (self.section_start >= other.section_start && self.section_end <= other.section_end)
    }

    fn overlapping(&self, other: &Section) -> bool {
        (self.section_start <= other.section_end && self.section_start >= other.section_start) ||
            (self.section_end >= other.section_start && self.section_end <= other.section_end) ||
            (other.section_start <= self.section_end && other.section_start >= self.section_start) ||
            (other.section_end >= self.section_start && other.section_end <= self.section_end)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    println!("In file {}", file_path);

    let contents: String = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let contents_split: Vec<Vec<Vec<&str>>> = contents.split('\n').collect::<Vec<&str>>().iter().
        map(|x| x.split(',').collect::<Vec<&str>>().
            iter().
            map(|x| x.split('-').
            collect()).
            collect()).
        collect();

    let elf_pairs_sections: Vec<ElfPair> = contents_split.iter().
        map(|x| ElfPair{pair: (
            Section{section_start: x[0][0].parse().unwrap(),
                section_end: x[0][1].parse().unwrap(),},
            Section{section_start: x[1][0].parse().unwrap(),
                section_end: x[1][1].parse().unwrap(),})}).
        collect();

    // let containing_sections: Vec<bool> = elf_pairs_sections.iter().
    //     map(|x| x.containing_section_assignments()).
    //     collect();

    let overlapping_sections: Vec<bool> = elf_pairs_sections.iter().
        map(|x| x.overlapping_section_assignments()).
        collect();

    let count = overlapping_sections.iter().filter(|x| **x==true).count();

    println!("{}", count);
}
