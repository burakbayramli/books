#!/usr/bin/env python

__author__ = "bt3"

import heapq

def find_N_largest_items_seq(seq, N):
    ''' find the N largest items in a sequence '''
    return heapq.nlargest(N,seq)

def find_N_smallest_items_seq(seq, N):
    ''' find the N smallest items in a sequence '''
    return heapq.nsmallest(N, seq)

def find_smallest_items_seq_heap(seq):
    ''' find the smallest items in a sequence using heapify first'''
    ''' heap[0] is always the smallest item '''
    ''' pops the smallest item, O(logN) '''
    heapq.heapify(seq)
    return heapq.heappop(seq)

def find_smallest_items_seq(seq):
    '''  if it is only one item, min() is faster '''
    return min(seq)

def find_N_smallest_items_seq_sorted(seq, N):
    '''  N ~ len(seq), better sort instead'''
    return sorted(seq)[:N]

def find_N_largest_items_seq_sorted(seq, N):
    '''  N ~ len(seq), better sort instead'''
    return sorted(seq)[len(seq)-N:]


def test_find_N_largest_smallest_items_seq(module_name='this module'):
    seq = [1, 3, 2, 8, 6, 10, 9]
    N = 3
    assert(find_N_largest_items_seq(seq, N) == [10, 9, 8])
    assert(find_N_largest_items_seq_sorted(seq, N) == [8, 9, 10])
    assert(find_N_smallest_items_seq(seq, N) == [1,2,3])
    assert(find_N_smallest_items_seq_sorted(seq, N) == [1,2,3])
    assert(find_smallest_items_seq(seq) == 1)
    assert(find_smallest_items_seq_heap(seq) == 1)

    s = 'Tests in {name} have {con}!'
    print(s.format(name=module_name, con='passed'))


if __name__ == '__main__':
    test_find_N_largest_smallest_items_seq()

