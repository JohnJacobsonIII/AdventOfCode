package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func campGroup(file_path string) int {
	file, err := os.Open(file_path)
	check_err(err)

	/* the defer keyword is used to delay the execution of a function
	or a statement until the nearby function returns.
	In simple words, defer will move the execution of the statement
	to the very end inside a function. */
	defer func(file *os.File) {
		err := file.Close()
		check_err(err)
	}(file)

	scanner := bufio.NewScanner(file)
	fully_contain := 0
	for scanner.Scan() {
		pair := strings.Split(scanner.Text(), ",")
		elf_1 := strings.Split(pair[0], "-")
		elf_1_lower, err := strconv.Atoi(elf_1[0])
		check_err(err)
		elf_1_upper, err := strconv.Atoi(elf_1[1])
		check_err(err)
		elf_2 := strings.Split(pair[1], "-")
		elf_2_lower, err := strconv.Atoi(elf_2[0])
		check_err(err)
		elf_2_upper, err := strconv.Atoi(elf_2[1])
		check_err(err)
		if elf_1_upper >= elf_2_upper && elf_2_upper >= elf_1_lower {
			fully_contain += 1
		} else if elf_2_upper >= elf_1_upper && elf_1_upper >= elf_2_lower {
			fully_contain += 1
		}
	}
	return fully_contain
}

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	fully_contain_pairs := campGroup("Day4/input")
	fmt.Println(fully_contain_pairs)
}
