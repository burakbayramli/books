#!/usr/bin/env python

__author__ = "bt3"


from collections import deque
from binary_search_tree import BST, Node
from binary_tree import BT, Node


def BFT(tree):
    current = tree.root
    nodes = []
    queue = deque()
    queue.append(current)

    while queue:
        current = queue.popleft()
        nodes.append(current.item)
        if current.left:
            queue.append(current.left)
        if current.right:
            queue.append(current.right)

    return nodes


def preorder(tree, nodes=None):
    nodes = nodes or []
    if tree:
        nodes.append(tree.item)
        if tree.left:
            preorder(tree.left, nodes)
        if tree.right:
            preorder(tree.right, nodes)
    return nodes


def postorder(tree, nodes=None):
    nodes = nodes or []
    if tree:
        if tree.left:
            nodes = postorder(tree.left, nodes)
        if tree.right:
            nodes = postorder(tree.right, nodes)
        nodes.append(tree.item)

    return nodes


def inorder(tree, nodes=None):
    nodes = nodes or []
    if tree:
        if tree.left:
            nodes = inorder(tree.left, nodes)
        nodes.append(tree.item)
        if tree.right:
            nodes = inorder(tree.right, nodes)
    return nodes


if __name__ == '__main__':


    # bt
    bt = BT()
    l = [10, 5, 6, 3, 8, 2, 1, 11, 9, 4]
    for i in l:
        bt.add(i)
    print 'BT...'
    print 'Original:    ', l
    print 'Preorder:    ', preorder(bt.root)
    print 'Postorder:   ', postorder(bt.root)
    print 'Inorder:     ', inorder(bt.root)
    print 'Breath:      ', BFT(bt)

    # bst
    bst = BST()
    l = [10, 5, 6, 3, 8, 2, 1, 11, 9, 4]
    for i in l:
        bst.add(i)

    print
    print 'BST ...'
    print 'Original:    ', l
    print 'Preorder:    ', preorder(bst.root)
    print 'Postorder:   ', postorder(bst.root)
    print 'Inorder:     ', inorder(bst.root)
    print 'Breath:      ', BFT(bst)
