#!/usr/bin/env python

__author__ = "bt3"

""" define a class for a set of stacks """

from stack import Stack

class SetOfStacks(Stack):
    def __init__(self, capacity=4):
        self.setofstacks = []
        self.items = []
        self.capacity = capacity


    def push(self, value):
        if self.size() >= self.capacity:
            self.setofstacks.append(self.items)
            self.items = []
        self.items.append(value)


    def pop(self):
        value = self.items.pop()
        if self.isEmpty() and self.setofstacks:
            self.items = self.setofstacks.pop()
        return value


    def sizeStack(self):
        return len(self.setofstacks)*self.capacity + self.size()


    def __repr__(self):
        aux = []
        for s in self.setofstacks:
             aux.extend(s)
        aux.extend(self.items)
        return '{}'.format(aux)



if __name__ == '__main__':
    capacity = 5
    stack = SetOfStacks(capacity)
    print("Is the stack empty? ", stack.isEmpty())
    print("Adding 0 to 10 in the stack...")
    for i in range(10):
        stack.push(i)
    print(stack)
    print("Stack size: ", stack.sizeStack())
    print("Stack peek : ", stack.peek())
    print("Pop...", stack.pop())
    print("Stack peek: ", stack.peek())
    print("Is the stack empty? ", stack.isEmpty())
    print(stack)
