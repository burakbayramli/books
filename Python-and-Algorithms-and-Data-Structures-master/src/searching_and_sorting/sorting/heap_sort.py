#!/usr/bin/env python

__author__ = "bt3"


''' Heapsort using Pythons libraries'''

import heapq

def heap_sort1(seq):
    ''' heap sort with Python's heapq '''
    h = []
    for value in seq:
        heapq.heappush(h, value)
    return [heapq.heappop(h) for i in range(len(h))]


def test_heap_sort1():
    seq = [3, 5, 2, 6, 8, 1, 0, 3, 5, 6, 2]
    assert(heap_sort1(seq) == sorted(seq))
    print('Tests passed!')



''' Heapsort using my Heap class '''

from heap import Heap

def heap_sort2(seq):
    heap = Heap(seq)

    res = []
    for i in range(len(seq)):
        res.insert(0, heap.extract_max())

    return res


def test_heap_sort2():
    seq = [3, 5, 2, 6, 8, 1, 0, 3, 5, 6, 2]
    print heap_sort2(seq)
    print('Tests passed!')


''' A third way of doing heap sort '''

def heap_sort3(seq):
    for start in range((len(seq)-2)//2, -1, -1):
        siftdown(seq, start, len(seq)-1)
    for end in range(len(seq)-1, 0, -1):
        seq[end], seq[0] = seq[0], seq[end]
        siftdown(seq, 0, end - 1)
    return seq

def siftdown(seq, start, end):
    root = start
    while True:
        child = root * 2 + 1
        if child > end: break
        if child + 1 <= end and seq[child] < seq[child + 1]:
            child += 1
        if seq[root] < seq[child]:
            seq[root], seq[child] = seq[child], seq[root]
            root = child
        else:
            break



def test_heap_sort3():
    seq = [3, 5, 2, 6, 8, 1, 0, 3, 5, 6, 2]
    assert(heap_sort3(seq) == sorted(seq))
    print('Tests passed!')







if __name__ == '__main__':
    test_heap_sort1()
    test_heap_sort2()
    test_heap_sort3()
