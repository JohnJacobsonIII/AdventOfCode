package main

import (
	"bufio"
	"golang.org/x/exp/slices"
	"os"
	"strings"
)

func groupOrg(file_path string) int {
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
	group := []string{}
	for scanner.Scan() {
		group = append(group, scanner.Text())
		if len(group) == 3 {
			elf_1 := strings.Split(group[0], "")
			elf_2 := strings.Split(group[1], "")
			elf_3 := strings.Split(group[2], "")
			for _, item := range elf_1 {
				if slices.Contains(elf_2, item) {
					if slices.Contains(elf_3, item) {
						item_prio := item_prior(item)
						total_prior += item_prio
						break
					}
				}
			}
			group = []string{}
		}
	}
	return total_prior
}
