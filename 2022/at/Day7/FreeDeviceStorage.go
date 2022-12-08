package main

import (
	"bufio"
	"fmt"
	"golang.org/x/exp/slices"
	"log"
	"os"
	"strconv"
	"strings"
)

type directory struct {
	name      string
	size      int
	files     []*subfile
	subdirs   []*directory
	parentdir *directory
}

type subfile struct {
	name      string
	size      int
	parentdir *directory
}

func freestorage(file_path string) int {
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
	outer_directory := directory{"/", 0, nil, nil, nil}
	var current_dir *directory
	for scanner.Scan() {
		command_line := strings.Split(scanner.Text(), " ")
		if slices.Contains(command_line, "cd") {
			if slices.Contains(command_line, "/") {
				current_dir = &outer_directory
			} else if slices.Contains(command_line, "..") {
				current_dir = current_dir.parentdir
			} else {
				subdir_name := command_line[2]
				isFound := false
				for _, dir := range current_dir.subdirs {
					if subdir_name == (*dir).name {
						current_dir = dir
						isFound = true
					}
				}
				if !isFound {
					fmt.Println("directory not found for cd", subdir_name)
					new_dir := directory{
						name:      subdir_name,
						size:      0,
						files:     nil,
						subdirs:   nil,
						parentdir: current_dir,
					}
					current_dir.subdirs = append(current_dir.subdirs, &new_dir)
					current_dir = &new_dir
				}
			}
			continue
		}
		if !slices.Contains(command_line, "$") {
			name := command_line[1]
			if slices.Contains(command_line, "dir") {
				isFound := contains(current_dir, name, true)
				if !isFound {
					new_dir := directory{
						name:      name,
						size:      0,
						files:     nil,
						subdirs:   nil,
						parentdir: current_dir,
					}
					current_dir.subdirs = append(current_dir.subdirs, &new_dir)
				}
			} else {
				isFound := contains(current_dir, name, false)
				if !isFound {
					file_size, err := strconv.Atoi(command_line[0])
					check_err(err)
					dir_file := subfile{name, file_size, current_dir}
					current_dir.files = append(current_dir.files, &dir_file)
					parent_dir := current_dir
					for parent_dir.name != "/" {
						parent_dir.size += file_size
						parent_dir = parent_dir.parentdir
					}
					outer_directory.size = outer_directory.size + file_size
				}
			}
		}
	}
	total_size := used_memory(&outer_directory)
	fmt.Println("total_size ", total_size)
	free_memory := 70000000 - total_size
	fmt.Println("free_memory ", free_memory)
	needed_memory := 30000000 - free_memory
	fmt.Println("needed_memory ", needed_memory)
	removed_dir_size := smallest_subdir_size(&outer_directory, needed_memory, total_size)
	return removed_dir_size
}

func smallest_subdir_size(dir *directory, needed_memory int, max_memory int) int {
	if dir.subdirs == nil {
		return dir.size
	}
	for _, subdir := range dir.subdirs {
		dir_mem := subdir.size
		fmt.Println(dir_mem)
		if dir_mem == needed_memory {
			max_memory = dir_mem
			return max_memory
		} else if dir_mem > needed_memory {
			if dir_mem < max_memory {
				max_memory = dir_mem
			}
			smallest_subdir_max_mem := smallest_subdir_size(subdir, needed_memory, max_memory)
			if smallest_subdir_max_mem == needed_memory {
				return smallest_subdir_max_mem
			}
			if smallest_subdir_max_mem < max_memory && smallest_subdir_max_mem > needed_memory {
				max_memory = smallest_subdir_max_mem
			}
		}
	}
	return max_memory
}
func dir_size(dir *directory) int {
	total_size := 0
	if dir == nil {
		return total_size
	}
	total_size += dir.size
	for _, subdir := range dir.subdirs {
		total_size += dir_size(subdir)
	}
	return total_size
}

func used_memory(dir *directory) int {
	total_size := 0
	if dir == nil {
		return total_size
	}
	for _, file := range dir.files {
		total_size += file.size
	}
	for _, subdir := range dir.subdirs {
		total_size += used_memory(subdir)
	}
	return total_size
}
func contains(dir *directory, name string, isDir bool) bool {
	if isDir {
		for _, subdir := range dir.subdirs {
			if name == subdir.name {
				return true
			}
		}
	} else {
		for _, subfile := range dir.files {
			if name == subfile.name {
				return true
			}
		}
	}

	return false
}
func check_err(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	removed_dir_size := freestorage("Day7/input")
	fmt.Println(removed_dir_size)
}
