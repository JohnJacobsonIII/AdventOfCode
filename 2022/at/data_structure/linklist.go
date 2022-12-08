package data_structure

import "fmt"

type (
	LinkList struct {
		head   *Linknode
		tail   *Linknode
		length int
	}
	Linknode struct {
		value interface{}
		next  *Linknode
	}
)

// Create a new stack
func NewLinkList() *LinkList {
	return &LinkList{nil, nil, 0}
}

// Return the number of items in the stack
func (this *LinkList) Len() int {
	return this.length
}

// Add a value to the end of the list
func (this *LinkList) Add(value interface{}) {
	n := &Linknode{value, nil}
	if this.length == 0 {
		this.head = n
		this.tail = n
		this.length++
	} else {
		this.tail.next = n
		this.tail = n
		this.length++
	}
}

// Return the node number that contain the value. If none then return 0
func (this *LinkList) Contain(value interface{}) (*Linknode, int) {
	node := this.head
	node_num := 0
	n := 0
	for n < this.length {
		node_num++
		if node.value == value {
			return node, node_num
		}
		n++
		node = node.next
	}
	return nil, 0
}

func (this *LinkList) SetLength(value int) {
	this.length = value
}

func (this *LinkList) Remove(node_to_remove *Linknode, num_nodes int) {
	this.head = node_to_remove.next
	this.length = this.length - num_nodes
}

func (this *LinkList) PrintList() {
	node := this.head
	nodes := ""
	n := 0
	for n < this.length {
		nodes += fmt.Sprintf("%v", node.value)
		node = node.next
		n++
	}
	fmt.Println(nodes)
}
