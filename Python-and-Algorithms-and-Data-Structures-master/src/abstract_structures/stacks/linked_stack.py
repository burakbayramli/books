#!/usr/bin/env python

__author__ = "bt3"


""" A stack made of linked list"""


class Node(object):
    def __init__(self, value=None, pointer=None):
        self.value = value
        self.pointer = pointer


class Stack(object):
    def __init__(self):
        self.head = None

    def isEmpty(self):
        return not bool(self.head)

    def push(self, item):
        self.head = Node(item, self.head)


    def pop(self):
        if self.head:
            node = self.head
            self.head = node.pointer
            return node.value
        else:
            print('Stack is empty.')


    def peek(self):
        if self.head:
            return self.head.value
        else:
            print('Stack is empty.')


    def size(self):
        node = self.head
        count = 0
        while node:
            count +=1
            node = node.pointer
        return count


    def _printList(self):
        node = self.head
        while node:
            print(node.value)
            node = node.pointer



if __name__ == '__main__':
    stack = Stack()
    print("Is the stack empty? ", stack.isEmpty())
    print("Adding 0 to 10 in the stack...")
    for i in range(10):
        stack.push(i)
    stack._printList()
    print("Stack size: ", stack.size())
    print("Stack peek : ", stack.peek())
    print("Pop...", stack.pop())
    print("Stack peek: ", stack.peek())
    print("Is the stack empty? ", stack.isEmpty())
    stack._printList()
