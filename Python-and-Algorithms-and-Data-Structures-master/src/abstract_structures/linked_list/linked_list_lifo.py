#!/usr/bin/env python

__author__ = "bt3"


''' Implement a unordered linked list, i.e. a LIFO linked list (like a stack) '''

from node import Node

class LinkedListLIFO(object):

    def __init__(self):
        self.head = None
        self.length = 0

    def _printList(self):
        node = self.head
        while node:
            print(node.value)
            node = node.pointer

    def _delete(self, prev, node):
        self.length -= 1
        if not prev:
            self.head = node.pointer
        else:
            prev.pointer = node.pointer

    def _add(self, value):
        self.length += 1
        self.head = Node(value, self.head)

    def _find(self, index):
        prev = None
        node = self.head
        i = 0
        while node and i < index:
            prev = node
            node = node.pointer
            i += 1
        return node, prev, i

    def _find_by_value(self, value):
        prev = None
        node = self.head
        found = 0
        while node and not found:
            if node.value == value:
                found = True
            else:
                prev = node
                node = node.pointer
        return node, prev, found

    def deleteNode(self, index):
        node, prev, i = self._find(index)
        if index == i:
            self._delete(prev, node)
        else:
            print('Node with index {} not found'.format(index))

    def deleteNodeByValue(self, value):
        node, prev, found = self._find_by_value(value)
        if found:
            self._delete(prev, node)
        else:
            print('Node with value {} not found'.format(value))




if __name__ == '__main__':
    ll = LinkedListLIFO()
    for i in range(1, 5):
        ll._add(i)
    print('The list is:')
    ll._printList()
    print('The list after deleting node with index 2:')
    ll.deleteNode(2)
    ll._printList()
    print('The list after deleting node with value 3:')
    ll.deleteNodeByValue(2)
    ll._printList()
    print('The list after adding node with value 15')
    ll._add(15)
    ll._printList()
    print("The list after deleting everything...")
    for i in range(ll.length-1, -1, -1):
        ll.deleteNode(i)
    ll._printList()
