#!/usr/bin/env python

__author__ = "bt3"

''' Implement a double-linked list, which is very simple, we just need inherits
from a Linked List Class and  add an attribute for previous.'''

from linked_list_fifo import LinkedListFIFO


class dNode(object):
    def __init__(self, value=None, pointer=None, previous=None):
        self.value = value
        self.pointer = pointer
        self.previous = previous


class dLinkList(LinkedListFIFO):

    def printListInverse(self):
        node = self.tail
        while node:
            print(node.value)
            try:
                node = node.previous
            except:
                break

    def _add(self, value):
        self.length += 1
        node = dNode(value)
        if self.tail:
            self.tail.pointer = node
            node.previous = self.tail
        self.tail = node

    def _delete(self, node):
        self.length -= 1
        node.previous.pointer = node.pointer
        if not node.pointer:
            self.tail = node.previous

    def _find(self, index):
        node = self.head
        i = 0
        while node and i < index:
            node = node.pointer
            i += 1
        return node, i

    # delete nodes in general
    def deleteNode(self, index):
        if not self.head or not self.head.pointer:
            self._deleteFirst()
        else:
            node, i = self._find(index)
            if i == index:
                self._delete(node)
            else:
                print('Node with index {} not found'.format(index))


if __name__ == '__main__':

    from collections import Counter

    ll = dLinkList()
    for i in range(1, 5):
        ll.addNode(i)
    print('Printing the list...')
    ll._printList()
    print('Now, printing the list inversely...')
    ll.printListInverse()
    print('The list after adding node with value 15')
    ll._add(15)
    ll._printList()
    print("The list after deleting everything...")
    for i in range(ll.length-1, -1, -1):
        ll.deleteNode(i)
    ll._printList()

