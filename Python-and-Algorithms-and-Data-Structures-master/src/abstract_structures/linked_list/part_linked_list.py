#!/usr/bin/env python

__author__ = "bt3"

''' This function divides a linked list in a value, where everything smaller than this value
    goes to the front, and everything large goes to the back:'''


from linked_list_fifo import LinkedListFIFO
from node import Node


def partList(ll, n):

    more = LinkedListFIFO()
    less = LinkedListFIFO()

    node = ll.head

    while node:
        item = node.value

        if item < n:
            less.addNode(item)

        elif item > n:
            more.addNode(item)

        node = node.pointer

    less.addNode(n)
    nodemore = more.head

    while nodemore:
        less.addNode(nodemore.value)
        nodemore = nodemore.pointer

    return less




if __name__ == '__main__':

    ll = LinkedListFIFO()
    l = [6, 7, 3, 4, 9, 5, 1, 2, 8]
    for i in l:
        ll.addNode(i)

    print('Before Part')
    ll._printList()

    print('After Part')
    newll = partList(ll, 6)
    newll._printList()


