#!/usr/bin/env python

__author__ = "bt3"

class Node(object):
    def __init__(self, value=None, pointer=None):
        self.value = value
        self.pointer = pointer

    def getData(self):
        return self.value

    def getNext(self):
        return self.pointer

    def setData(self, newdata):
        self.value = newdata

    def setNext(self, newpointer):
        self.pointer = newpointer



if __name__ == '__main__':
    L = Node("a", Node("b", Node("c", Node("d"))))
    assert(L.pointer.pointer.value=='c')

    print(L.getData())
    print(L.getNext().getData())
    L.setData('aa')
    L.setNext(Node('e'))
    print(L.getData())
    print(L.getNext().getData())
