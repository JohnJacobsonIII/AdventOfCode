package main

import (
	"bufio"
	"fmt"
	"golang.org/x/exp/slices"
	"log"
	"os"
	"strings"
)

var items_list = [...]string{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
	"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"}

func rucksackReorg(file_path string) int {
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
	total_prior := 0
	for scanner.Scan() {
		rucksack := scanner.Text()
		mid_point := len(rucksack) / 2
		sack_1 := strings.Split(rucksack[:mid_point], "")
		sack_2 := strings.Split(rucksack[mid_point:], "")
		for _, item := range sack_1 {
			if slices.Contains(sack_2, item) {
				item_prio := item_prior(item)
				total_prior += item_prio
				break
			}
		}
	}
	return total_prior
}

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func item_prior(item string) int {
	for index, element := range items_list {
		if element == item {
			return index + 1
		}
	}
	return -1
}

func main() {
	total_prior := groupOrg("Day3/input")
	fmt.Println(total_prior)
}
