#!/usr/bin/env python

__author__ = "bt3"


import random

''' The simplest way...'''
def quickSelect(seq, k):
    # this part is the same as quick sort
    len_seq = len(seq)
    if len_seq < 2: return seq

    # we could use a random choice here doing
    #pivot = random.choice(seq)
    ipivot = len_seq // 2
    pivot = seq[ipivot]

    # O(n)
    smallerList = [x for i,x in enumerate(seq) if x <= pivot and  i != ipivot]
    largerList = [x for i,x in enumerate(seq) if x > pivot and  i != ipivot]

    # here starts the different part
    m = len(smallerList)
    if k == m:
        return pivot
    elif k < m:
        return quickSelect(smallerList, k)
    else:
        return quickSelect(largerList, k-m-1)



''' If you don't want to use pythons feature at all and
    also select pivot randomly'''

def swap(seq, x, y):
    tmp = seq[x]
    seq[x] = seq[y]
    seq[y] = tmp


def quickSelectHard(seq, k, left=None, right=None):
    left = left or 0
    right = right or len(seq) - 1
    #ipivot = random.randint(left, right)
    ipivot = len(seq)//2
    pivot = seq[ipivot]

    # Move pivot out of the sorting range
    swap(seq, ipivot, right)
    swapIndex, i = left, left
    while i < right:
        if seq[i] < pivot:
            swap(seq, i, swapIndex)
            swapIndex += 1
        i += 1

    # Move pivot to final position
    swap(seq, right, swapIndex)

    # Check if pivot matches, else recurse on the correct half
    rank = len(seq) - swapIndex


    if k == rank:
        return seq[swapIndex]
    elif k < rank:
        return quickSelectHard(seq, k, swapIndex+1, right)
    else:
        return quickSelectHard(seq, k, left, swapIndex-1)




if __name__ == '__main__':
    # Checking the Answer
    seq = [10, 60, 100, 50, 60, 75, 31, 50, 30, 20, 120, 170, 200]
    #seq = [3, 7, 2, 1, 4, 6, 5, 10, 9, 11]

    # we want the middle element
    k = len(seq) // 2

    # Note that this only work for odd arrays, since median in
    # even arrays is the mean of the two middle elements
    print(quickSelect(seq, k))
    print(quickSelectHard(seq, k))
    import numpy
    print numpy.median(seq)