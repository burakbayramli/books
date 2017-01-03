#!/usr/bin/env python

__author__ = "bt3"


class Node(object):
    def __init__(self, value, next=None):
        self.value = value
        self.next = next
        

class LinkedList(object):
    def __init__(self):
        self.head = None
  
      def _add(self, value):
        self.head = Node(value, self.head)
            
    def _printList(self):
        node = self.head
        while node:
            print node.value
            node = node.next
            
    def _find(self, index):
        prev = None
        node = self.head
        i = 0
        while node and i < index:
            prev = node
            node = node.next
            i += 1
        return node, prev, i

    def _delete(self, prev, node):
        if not prev:
            self.head = node.next
        else:
            prev.next = node.next
    
    def deleteNode(self, index):
        node, prev, i = self._find(index)
        if index == i:
            self._delete(prev, node)
        else:
            print('Node with index {} not found'.format(index))

    

if __name__ == '__main__':
    ll = LinkedList()
    for i in range(1, 5):
        ll._add(i)
        
    print('The list is:')
    ll._printList()
     
    print('The list after deleting node with index 2:')
    ll.deleteNode(2)
    ll._printList()