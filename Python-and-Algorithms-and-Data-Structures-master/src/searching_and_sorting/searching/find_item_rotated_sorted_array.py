#!/usr/bin/env python

__author__ = "bt3"



'''
Given a sorted array that was rotated, find an item with binary search:
'''

def find_element_rot_array(seq, key, lo=0, hi=None):

    hi = hi or len(seq)
    if hi <= lo:
        return None # base case: <= for odd and even numbers!

    mid = (hi + lo) // 2

    if key == seq[mid]:
        return mid

    # if left is ordered --> we work here
    if seq[lo] <= seq[mid]:

        # now, is the key there?
        if key < seq[mid] and key >= seq[lo]:
            return find_element_rot_array(seq, key, lo, mid)
        else:
        # all the other cases
            return find_element_rot_array(seq, key, mid+1, hi)

    # right is ordered --> we work here
    else:

        # now, is the key there?
        if key > seq[mid] and key <= seq[hi-1]: # stupid hi-1!!!
            return find_element_rot_array(seq, key, mid+1, hi)
        else:
        # all the other cases
            return find_element_rot_array(seq, key, lo, mid)


def test_find_element_rot_array():
    l1 = [3, 4, 5, 6, 7, 1, 2]
    assert(find_element_rot_array(l1, 7) == 4 )
    print("Tests passed!")


if __name__ == '__main__':
    test_find_element_rot_array()

