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

        else:
            if value > self.item:
                self.right = self.right and self.right._add(value) or new_node
            elif value < self.item:
                self.left = self.left and self.left._add(value) or new_node
            else:
                print("BSTs do not support repeated items.")

        return self # this is necessary!!!


    def _search(self, value):
        if self.item == value:
            return True # or self

        elif self.left and value < self.item:
                return self.left._search(value)

        elif self.right and value > self.item:
                return self.right._search(value)

        else:
            return False


    def _isLeaf(self):
        return not self.right and not self.left


    def _printPreorder(self):
        print self.item

        if self.left:
            self.left._printPreorder()

        if self.right:
            self.right._printPreorder()

    # Another possibility: use an array (a little bit more expensive):
    def _preorder_array(self):
        nodes = []
        if self.item:
            nodes.append(self.item)
        if self.left:
            nodes.extend(self.left._preorder_array())
        if self.right:
            nodes.extend(self.right._preorder_array())
        return nodes




class BST(object):

    def __init__(self):
        self.root = None

    def add(self, value):
        if not self.root:
            self.root = Node(value)
        else:
            self.root._add(value)

    def printPreorder(self):
        if self.root:
            self.root._printPreorder()

    def search(self, value):
        if self.root:
            return self.root._search(value)

    def preorder_array(self):
        if self.root:
            return self.root._preorder_array()
        else:
            return 'Tree is empty.'


if __name__ == '__main__':

    bst = BST()
    print "Adding nodes 1 to 10 in the tree..."
    for i in range(1, 11):
        bst.add(i)

    print
    print "Searching for nodes 16 and 6"
    print bst.search(16)
    print bst.search(6)

    print
    print "Printing preorder..."
    bst.printPreorder()




