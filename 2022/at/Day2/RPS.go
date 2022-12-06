package main

import (
	"bufio"
	"log"
	"os"
	"strings"
)

type HandSign interface {
	CompareResult(opponentSign string)
}

type abstractSign struct {
	HandSign
	value         int
	weakAgainst   string
	strongAgainst string
}

func (handSign abstractSign) CompareResult(opponentSign string) int {
	roundScore := handSign.value
	if handSign.weakAgainst == opponentSign {
		return roundScore
	} else if handSign.strongAgainst == opponentSign {
		roundScore += 6
	} else {
		roundScore += 3
	}
	return roundScore
}

func rps(file_path string) int {
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
		yourSign := signs[1]
		sign := abstractSign{
			HandSign:      nil,
			value:         0,
			weakAgainst:   "",
			strongAgainst: "",
		}
		if yourSign == "X" {
			sign = abstractSign{value: 1, weakAgainst: "B", strongAgainst: "C"}
		} else if yourSign == "Y" {
			sign = abstractSign{value: 2, weakAgainst: "C", strongAgainst: "A"}
		} else {
			sign = abstractSign{value: 3, weakAgainst: "A", strongAgainst: "B"}
		}
		score := sign.CompareResult(opponentSign)
		final_score += score
	}
	return final_score
}

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
