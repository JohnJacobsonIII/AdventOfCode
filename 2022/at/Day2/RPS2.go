package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func rps2(file_path string) int {
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

	final_score := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		signs := strings.Split(scanner.Text(), " ")
		opponentSign := signs[0]
		result := signs[1]
		score := compare(opponentSign, result)
		final_score += score
	}
	return final_score
}

func compare(opponent_sign string, result string) int {
	roundScore := 0
	if opponent_sign == "A" {
		if result == "X" {
			roundScore = 3
		} else if result == "Y" {
			roundScore = 4
		} else {
			roundScore = 8
		}
	} else if opponent_sign == "B" {
		if result == "X" {
			roundScore = 1
		} else if result == "Y" {
			roundScore = 5
		} else {
			roundScore = 9
		}
	} else {
		if result == "X" {
			roundScore = 2
		} else if result == "Y" {
			roundScore = 6
		} else {
			roundScore = 7
		}
	}
	return roundScore
}

func main() {
	final_score := rps2("Day2/input")
	fmt.Println(final_score)
}
