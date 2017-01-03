#!/usr/bin/env python

__author__ = "bt3"


class Node(object):
    def __init__(self, value):
        self.value = value
        self.right = None
        self.left = None

    def add(self, value):
        new_node = Node(value)
        if not self.value:
            self.value = new_node
        elif not self.left:
            self.left = new_node
        elif not self. right:
            self.right = new_node
        else:
            self.left = self.left.add(value)
        return self # without this, it doesn't add!

    def search(self, item):
        if self.value == item:
            return True
        found = False
        if (self.left and self.left.search(item)) or \
                (self.right and self.right.search(item)):
            found = True
        return found

    def preorder(self):
        yield self.value
        if self.left:
            for node in self.left.preorder():
                yield node
        if self.right:
            for node in self.right.preorder():
                yield node

    def postorder(self):
        yield self.value
        if self.left:
            for node in self.left.postorder():
                yield node
        if self.right:
            for node in self.right.postorder():
                yield node

    def inorder(self):
        yield self.value
        if self.left:
            for node in self.left.inorder():
                yield node
        if self.right:
            for node in self.right.inorder():
                yield node

    # this is the most basic way to write this function
    def preorder_simple(self):
        print self.value
        if self.left:
            self.left.preorder_simple()
        if self.right:
            self.right.preorder_simple()


    # Another possibility: use an array (a little bit more expensive):
    def preorder_array(self):
        nodes = []
        if self.value:
            nodes.append(self.value)
        if self.left:
            nodes.extend(self.left.preorder_array())
        if self.right:
            nodes.extend(self.right.preorder_array())
        return nodes



class BT(object):
    def __init__(self):
        self.root = None

    def add(self, value):
        if not self.root:
            self.root = Node(value)
        else:
            self.root.add(value)

    def search(self, item):
        if self.root:
            return self.root.search(item)
        else:
            return 'Tree is empty.'

    def preorder(self):
        if self.root:
            return self.root.preorder()
        else:
            return 'Tree is empty.'

    def inorder(self):
        if self.root:
            return self.root.inorder()
        else:
            return 'Tree is empty.'

    def postorder(self):
        if self.root:
            return self.root.postorder()
        else:
            return 'Tree is empty.'

    def preorder_array(self):
        if self.root:
            return self.root.preorder_array()
        else:
            return 'Tree is empty.'

    def preorder_simple(self):
        if self.root:
            return self.root.preorder_simple()
        else:
            return 'Tree is empty.'


if __name__ == '__main__':
    tree = BT()

    for i in range(1, 11):
        tree.add(i)

    print 'Searching for node 4'
    print tree.search(4)

    print 'Searching for node 1'
    print tree.search(1)

    print 'Searching for node 12'
    print tree.search(12)

    print 'Pre-order generator...'
    getree = tree.preorder()
    for i in range(10):
        print next(getree)
    print
    print 'Pre-order array...'

    print tree.preorder_array()

    print
    print 'Pre-order simple...'

    tree.preorder_simple()

    print
    print 'Inorder...'

    getree = tree.inorder()
    for i in range(10):
        print next(getree)

    print
    print 'Postorder...'

    getree = tree.postorder()
    for i in range(10):
        print next(getree)
