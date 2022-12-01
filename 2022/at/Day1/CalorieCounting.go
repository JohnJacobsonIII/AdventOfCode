package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func calorie_count(file_path string) [3]int {
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

	top_three_cal := [3]int{0, 0, 0}
	single_elf_calories := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		snack := scanner.Text()
		if snack == "" {
			if single_elf_calories > top_three_cal[2] {
				top_three_cal[1] = top_three_cal[2]
				top_three_cal[2] = single_elf_calories
			} else if single_elf_calories > top_three_cal[1] {
				top_three_cal[0] = top_three_cal[1]
				top_three_cal[1] = single_elf_calories
			} else if single_elf_calories > top_three_cal[0] {
				top_three_cal[0] = single_elf_calories
			}
			single_elf_calories = 0
		} else {
			calorie, err := strconv.Atoi(snack)
			check_err(err)
			single_elf_calories += calorie
		}
	}
	return top_three_cal
}

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	top_three_calories := calorie_count("Day1/input_calories")
	highest_calorie := top_three_calories[2]
	total_calorie := 0
	for _, v := range top_three_calories {
		total_calorie += v
	}
	fmt.Println("Total calorie is", total_calorie)
	fmt.Println("Highest calorie is", highest_calorie)
}
