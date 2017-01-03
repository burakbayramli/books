#!/usr/bin/env python

__author__ = "bt3"



def ordered_sequential_search(seq, n):
    ''' an example of sequential search in an ordered seq:
        it improves the performance if the item is not present  '''
    item = 0
    for item in seq:
        if item > n: return False
        if item == n: return True
    return False


def test_ordered_sequential_search(module_name='this module'):
    seq = [1, 2, 4, 5, 6, 8, 10]
    n1 = 10
    n2 = 7
    assert(ordered_sequential_search(seq, n1) == True)
    assert(ordered_sequential_search(seq, n2) == False)

    s = 'Tests in {name} have {con}!'
    print(s.format(name=module_name, con='passed'))


if __name__ == '__main__':
    test_ordered_sequential_search()


"""
Case	            Best Case	    Worst Case	    Average Case
item is present	        1	            n	            n/2
item not present	    1	            n	            n/2
"""
