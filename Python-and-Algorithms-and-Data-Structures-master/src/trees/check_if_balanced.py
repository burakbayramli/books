#!/usr/bin/env python

__author__ = "bt3"

from binary_search_tree import BST, Node
from binary_tree import BT, Node



def isBalanced(node, left=0, right=0):
    if not node:
        return (left - right) < 2

    return isBalanced(node.left, left+1, right) and \
        isBalanced(node.right, left, right+1)




if __name__ == '__main__':
    bt = BST()
    for i in range(1, 10):
        bt.add(i)

    assert(isBalanced(bt.root) == True)

    bt = BT()
    for i in range(1, 10):
        bt.add(i)

    assert(isBalanced(bt.root) == False)
