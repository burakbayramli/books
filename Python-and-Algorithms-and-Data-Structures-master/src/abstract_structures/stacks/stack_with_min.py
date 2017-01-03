#!/usr/bin/env python

__author__ = "bt3"

''' A stack with a minimum lookup '''

from stack import Stack


class NodeWithMin(object):
    def __init__(self, value=None, minimum=None):
        self.value = value
        self.minimum = minimum


class StackMin(Stack):
    def __init__(self):
        self.items = []
        self.minimum = None


    def push(self, value):
        if self.isEmpty() or self.minimum > value:
           self.minimum = value
        self.items.append(NodeWithMin(value, self.minimum))


    def peek(self):
        return self.items[-1].value


    def peekMinimum(self):
        return self.items[-1].minimum


    def pop(self):
        item = self.items.pop()
        if item:
            if item.value == self.minimum:
                self.minimum = self.peekMinimum()
            return item.value
        else:
            print("Stack is empty.")

    def __repr__(self):
        aux = []
        for i in self.items:
            aux.append(i.value)
        return '{}'.format(aux)



if __name__ == '__main__':
    stack = StackMin()
    print("Is the stack empty? ", stack.isEmpty())
    print("Adding 0 to 10 in the stack...")
    for i in range(10,3, -1):
        stack.push(i)
    print(stack)

    print("Stack size: ", stack.size())
    print("Stack peek and peekMinimum : ", stack.peek(), stack.peekMinimum())
    print("Pop...", stack.pop())
    print("Stack peek and peekMinimum : ", stack.peek(), stack.peekMinimum())
    print("Is the stack empty? ", stack.isEmpty())
    print(stack)

    for i in range(5, 1, -1):
        stack.push(i)
    print(stack)

    print("Stack size: ", stack.size())
    print("Stack peek and peekMinimum : ", stack.peek(), stack.peekMinimum())
    print("Pop...", stack.pop())
    print("Stack peek and peekMinimum : ", stack.peek(), stack.peekMinimum())
    print("Is the stack empty? ", stack.isEmpty())
    print(stack)
