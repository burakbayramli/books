#!/usr/bin/env python

__author__ = "bt3"


from binary_search_tree import BST, Node

def find_ancestor(path, low_item, high_item):
    while path:
        current_item = path[0]

        if current_item < low_item:
            try:
                path = path[2:]
            except:
                return current_item

        elif current_item > high_item:
            try:
                path = path[1:]
            except:
                return current_item

        elif low_item <= current_item <= high_item:
            return current_item


def find_ancestor2(tree, n1, n2):
    if not tree:
        return False

    if n1 <= tree.item and n2 >= tree.item or (not tree.left and not tree.right) :
        return tree.item

    if tree.left and (n1 < tree.item and n2 < tree.item):
        return find_ancestor(tree.left, n1, n2) or tree.item

    if tree.right and (n1 > tree.item and n2 > tree.item):
        return find_ancestor(tree.right, n1, n2) or tree.item



if __name__ == '__main__':
    bst = BST()
    l = [10, 5, 6, 3, 8, 2, 1, 11, 9, 4]
    for i in l:
        bst.add(i)
    nodes = bst.preorder_array()

    print 'Original:    ', l
    print 'Preorder:    ', nodes

    print 'Method 1: '
    print 'Ancestor for 3, 11:', find_ancestor(nodes, 3, 11)

    print 'Method 2: '
    print 'Ancestor for 3, 11: ', find_ancestor2(bst.root, 3, 11)
