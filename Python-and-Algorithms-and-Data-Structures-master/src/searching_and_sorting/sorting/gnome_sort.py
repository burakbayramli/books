#!/usr/bin/env python

__author__ = "bt3"


def gnome_sort(seq):
    ''' sort a sequence using the gnome sort alg '''
    i = 0
    while i < len(seq):
        if i ==0 or seq[i-1] <= seq[i]:
            i += 1
        else:
            seq[i], seq[i-1] = seq[i-1], seq[i]
            i -= 1
    return seq


def test_gnome_sort():
    seq = [3, 5, 2, 6, 8, 1, 0, 3, 5, 6, 2, 5, 4, 1, 5, 3]
    assert(gnome_sort(seq) == sorted(seq))


if __name__ == '__main__':
    test_gnome_sort()







