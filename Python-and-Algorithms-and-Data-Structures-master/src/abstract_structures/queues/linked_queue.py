#!/usr/bin/env python

__author__ = "bt3"

''' Queue acts as a container for nodes (objects) that are inserted and removed according FIFO'''


class Node(object):
    def __init__(self, value=None, pointer=None):
        self.value = value
        self.pointer = None


class LinkedQueue(object):
    def __init__(self):
        self.head = None
        self.tail = None


    def isEmpty(self):
        return not bool(self.head)


    def dequeue(self):
        if self.head:
            value = self.head.value
            self.head = self.head.pointer
            return value
        else:
            print('Queue is empty, cannot dequeue.')


    def enqueue(self, value):
        node = Node(value)
        if not self.head:
            self.head = node
            self.tail = node
        else:
            if self.tail:
                self.tail.pointer = node
            self.tail = node


    def size(self):
        node = self.head
        num_nodes = 0
        while node:
                num_nodes += 1
                node = node.pointer
        return num_nodes


    def peek(self):
        return self.head.value


    def _print(self):
        node = self.head
        while node:
            print(node.value)
            node = node.pointer




if __name__ == '__main__':
    queue = LinkedQueue()
    print("Is the queue empty? ", queue.isEmpty())
    print("Adding 0 to 10 in the queue...")
    for i in range(10):
        queue.enqueue(i)
    print("Is the queue empty? ", queue.isEmpty())
    queue._print()

    print("Queue size: ", queue.size())
    print("Queue peek : ", queue.peek())
    print("Dequeue...", queue.dequeue())
    print("Queue peek: ", queue.peek())
    queue._print()

