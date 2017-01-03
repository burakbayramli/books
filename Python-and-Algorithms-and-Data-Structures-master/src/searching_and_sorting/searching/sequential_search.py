#!/usr/bin/env python

__author__ = "bt3"


def sequential_search(seq, n):
    ''' an example of sequential search (for items stored in a collection) '''
    for item in seq:
        if item == n: return True
    return False


def test_sequential_search(module_name='this module'):
    seq = [1, 5, 6, 8, 3]
    n1 = 5
    n2 = 7
    assert(sequential_search(seq, n1) == True)
    assert(sequential_search(seq, n2) == False)

    s = 'Tests in {name} have {con}!'
    print(s.format(name=module_name, con='passed'))


if __name__ == '__main__':
    test_sequential_search()


"""
Case	            Best Case	    Worst Case	    Average Case
item is present	        1           	n	            n2
item is not present	    n	            n           	n
"""