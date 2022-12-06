package data_structure

import (
	"fmt"
)

type (
	Stack struct {
		top    *node
		length int
	}
	node struct {
		value interface{}
		prev  *node
	}
)

// Create a new stack
func New() *Stack {
	return &Stack{nil, 0}
}

// Return the number of items in the stack
func (this *Stack) Len() int {
	return this.length
}

// View the top item on the stack
func (this *Stack) Peek() interface{} {
	if this.length == 0 {
		return nil
	}
	return this.top.value
}

// Pop the top item of the stack and return it
func (this *Stack) Pop() interface{} {
	if this.length == 0 {
		return nil
	}

	n := this.top
	this.top = n.prev
	this.length--
	return n.value
}

// Push a value onto the top of the stack
func (this *Stack) Push(value interface{}) {
	n := &node{value, this.top}
	this.top = n
	this.length++
}

func (this *Stack) Push_n(values []string) {
	for _, v := range values {
		this.Push(v)
	}
}

func (this *Stack) Pop_n(num_pop int) []string {
	n := 0
	items := []string{}
	for n < num_pop {
		item := fmt.Sprintf("%v", this.Pop())
		items = append(items, item)
		n += 1
	}
	return items
}
