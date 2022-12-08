package main

import (
	iter "at/data_structure"
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func tuning(file_path string) int {
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
	link_list := iter.NewLinkList()
	location := 0
	for scanner.Scan() {
		message := scanner.Text()
		message_arr := strings.Split(message, "")
		for _, char := range message_arr {
			node_contain, node_num := link_list.Contain(char)
			if node_num > 0 {
				link_list.Remove(node_contain, node_num)
			}
			link_list.Add(char)
			location++
			link_list.PrintList()
			if link_list.Len() == 14 {
				return location
			}

		}
	}
	return location
}

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	first_mark := tuning("Day6/input")
	fmt.Println(first_mark)
}
