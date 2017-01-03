#!/usr/bin/env python

__author__ = "bt3"


from binary_search_tree import BST, Node

def largest(node):

    if node.right:
        return largest(node.right)
    return node.item


if __name__ == '__main__':


    bst = BST()
    l = [10, 5, 6, 3, 8, 2, 1, 11, 9, 4]
    for i in l:
        bst.add(i)

    print(largest(bst.root))
