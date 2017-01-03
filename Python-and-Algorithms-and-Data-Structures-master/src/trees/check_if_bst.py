#!/usr/bin/env python

__author__ = "bt3"


from binary_search_tree import BST, Node
from binary_tree import BT, Node


def isBST(node, min_node=float("-infinity"), maxVal=float("infinity")):
    if not node:
        return True

    if not min_node <= node.item <= maxVal:
        return False

    return isBST(node.left, min_node, node.item) and \
           isBST(node.right, node.item, maxVal)



def isBST_other_method(node, max_node=None, min_node=None):

    if not node:
        return True

    left, right =  True, True
    min_node = min_node or float('inf')
    max_node = max_node or -float('inf')

    if node.left:
        if node.left.item > node.item or node.left.item > max_node:
            left = False
        else:
            max_node = node.item
            left =  isBST(node.left, max_node, min_node)

    if node.right:
        if node.right.item < node.item or node.right.item < min_node:
            rihjt =  False
        else:
            min_node = node.item
            right =  isBST(node.right, max_node, min_node)

    return left and right





if __name__ == '__main__':
    bt = BST()
    for i in range(1, 10):
        bt.add(i)

    assert(isBST(bt.root) == True)

    bt = BT()
    for i in range(1, 10):
        bt.add(i)

    assert(isBST(bt.root) == False)


