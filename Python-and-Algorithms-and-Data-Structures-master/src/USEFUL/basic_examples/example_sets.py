#!/usr/bin/env python

__author__ = "bt3"

def difference(l1):
    """ return the list with duplicate elements removed """
    return list(set(l1))

def intersection(l1, l2):
    """ return the intersection of two lists """
    return list(set(l1) & set(l2))

def union(l1, l2):
    """ return the union of two lists """
    return list(set(l1) | set(l2))


def test_sets_operations_with_lists():
    l1 = [1,2,3,4,5,9,11,15]
    l2 = [4,5,6,7,8]
    l3 = []
    assert(difference(l1) == [1, 2, 3, 4, 5, 9, 11, 15])
    assert(difference(l2) == [8, 4, 5, 6, 7])
    assert(intersection(l1, l2) == [4,5])
    assert(union(l1, l2) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 15])
    assert(difference(l3) == [])
    assert(intersection(l3, l2) == l3)
    assert(sorted(union(l3, l2)) == sorted(l2))
    print('Tests passed!')


if __name__ == '__main__':
    test_sets_operations_with_lists()




