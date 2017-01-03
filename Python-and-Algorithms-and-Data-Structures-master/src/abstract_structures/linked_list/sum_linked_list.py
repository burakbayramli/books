#!/usr/bin/env python

__author__ = "bt3"

''' Supposing two linked lists representing numbers, such that in each of their
    nodes they carry one digit. This function sums the two numbers that these
    two linked lists represent, returning a third list representing the sum:'''


from linked_list_fifo import LinkedListFIFO
from node import Node


class LinkedListFIFOYield(LinkedListFIFO):

    # print each node's value, starting from the head
    def _printList(self):
        node = self.head
        while node:
            yield(node.value)
            node = node.pointer



def sumlls(l1, l2):

    lsum = LinkedListFIFOYield()
    dig1 = l1.head
    dig2 = l2.head
    pointer = 0

    while dig1 and dig2:
        d1 = dig1.value
        d2 = dig2.value
        sum_d = d1 + d2 + pointer
        if sum_d > 9:
            pointer = sum_d//10
            lsum.addNode(sum_d%10)

        else:
            lsum.addNode(sum_d)
            pointer = 0

        dig1 = dig1.pointer
        dig2 = dig2.pointer

    if dig1:
        sum_d = pointer + dig1.value
        if sum_d > 9:
            lsum.addNode(sum_d%10)
        else:
            lsum.addNode(sum_d)
        dig1 = dig1.pointer

    if dig2:
        sum_d = pointer + dig2.value
        if sum_d > 9:
            lsum.addNode(sum_d%10)
        else:
            lsum.addNode(sum_d)
        dig2 = dig2.pointer

    return lsum






if __name__ == '__main__':
    l1 = LinkedListFIFOYield() # 2671
    l1.addNode(1)
    l1.addNode(7)
    l1.addNode(6)
    l1.addNode(2)

    l2 = LinkedListFIFOYield() # 455
    l2.addNode(5)
    l2.addNode(5)
    l2.addNode(4)

    lsum = sumlls(l1, l2)
    l = list(lsum._printList())
    for i in reversed(l):
        print i
