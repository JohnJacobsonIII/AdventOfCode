package main

import (
	stack "at/data_structure"
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func supplyStack(file_path string) string {
	stack_1 := stack.New()
	stack_1.Push("T")
	stack_1.Push("D")
	stack_1.Push("W")
	stack_1.Push("Z")
	stack_1.Push("V")
	stack_1.Push("P")
	stack_2 := stack.New()
	stack_2_items := []string{"L", "S", "W", "V", "F", "J", "D"}
	stack_2.Push_n(stack_2_items)
	stack_3 := stack.New()
	stack_3_items := []string{"Z", "M", "L", "S", "V", "T", "B", "H"}
	stack_3.Push_n(stack_3_items)
	stack_4 := stack.New()
	stack_4_items := []string{"R", "S", "J"}
	stack_4.Push_n(stack_4_items)
	stack_5 := stack.New()
	stack_5_items := []string{"C", "Z", "B", "G", "F", "M", "L", "W"}
	stack_5.Push_n(stack_5_items)
	stack_6 := stack.New()
	stack_6_items := []string{"Q", "W", "V", "H", "Z", "R", "G", "B"}
	stack_6.Push_n(stack_6_items)
	stack_7 := stack.New()
	stack_7_items := []string{"V", "J", "P", "C", "B", "D", "N"}
	stack_7.Push_n(stack_7_items)
	stack_8 := stack.New()
	stack_8_items := []string{"P", "T", "B", "Q"}
	stack_8.Push_n(stack_8_items)
	stack_9 := stack.New()
	stack_9_items := []string{"H", "G", "Z", "R", "C"}
	stack_9.Push_n(stack_9_items)
	ship := [9]*stack.Stack{stack_1, stack_2, stack_3, stack_4, stack_5, stack_6, stack_7, stack_8, stack_9}

	//stack_2 := stack.New()
	//stack_2_items := []string{"Z", "N"}
	//stack_2.Push_n(stack_2_items)
	//stack_3 := stack.New()
	//stack_3_items := []string{"M", "C", "D"}
	//stack_3.Push_n(stack_3_items)
	//stack_4 := stack.New()
	//stack_4_items := []string{"P"}
	//stack_4.Push_n(stack_4_items)
	//ship := [3]*stack.Stack{stack_2, stack_3, stack_4}

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
	for scanner.Scan() {
		move := strings.Split(scanner.Text(), " ")
		num_items, err := strconv.Atoi(move[1])
		check_err(err)
		from, err := strconv.Atoi(move[3])
		check_err(err)
		to, err := strconv.Atoi(move[5])
		check_err(err)
		items_moved := ship[from-1].Pop_n(num_items)
		items_moved = reverse(items_moved)
		ship[to-1].Push_n(items_moved)
	}
	final_configure := ""
	for _, item_stack := range ship {
		top_stack := fmt.Sprintf("%v", item_stack.Pop())
		final_configure += top_stack
	}
	return final_configure
}

func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func reverse(input []string) []string {
	for i, j := 0, len(input)-1; i < j; i, j = i+1, j-1 {
		input[i], input[j] = input[j], input[i]
	}
	return input
}

func main() {
	final_configure := supplyStack("Day5/input")
	fmt.Println(final_configure)
}
