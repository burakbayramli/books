#!/usr/bin/python

__author__ = "bt3"


class Node(object):

    def __init__(self, item=None,):
        self.item = item
        self.left = None
        self.right = None

    def __repr__(self):
        return '{}'.format(self.item)


    def _add(self, value):
        new_node = Node(value)

        if not self.item:
            self.item = new_node
        elif not self.left:
            self.left = new_node
        elif not self.right:
            self.right = new_node
        else:
            self.left = self.left._add(value)

        return self


    def _search(self, value):
        if self.item == value:
            return True # or self

        found = False # or None, thats diff from BST
        if self.left:
            found = self.left._search(value)

        if self.right:
            found =  found or self.right._search(value)

        return found


    def _isLeaf(self):
        return not self.right and not self.left


    def _preorder(self):
        print self.item
        if self.left:
            self.left._preorder()
        if self.right:
            self.right._preorder()



class BT(object):

    def __init__(self):
        self.root = None


    def add(self, value):
        if not self.root:
            self.root = Node(value)
        else:
            self.root._add(value)


    def search(self, value):
        if self.root:
            return self.root._search(value)


    def preorder(self):
        if self.root:
            return self.root._preorder()


if __name__ == '__main__':

    bt = BT()
    print "Adding nodes 1 to 10 in the tree..."
    for i in range(1, 11):
        bt.add(i)

    print
    print "Searching for nodes 16 and 6"
    print bt.search(16)
    print bt.search(6)

    print
    print "Printing preorder..."
    bt.preorder()
