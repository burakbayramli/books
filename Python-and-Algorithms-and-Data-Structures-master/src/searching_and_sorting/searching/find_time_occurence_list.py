#!/usr/bin/env python

__author__ = "bt3"

def binary_serch_counting(lst1, k, lo=0, hi=None):
    if hi is None: hi = len(lst1)
    while lo < hi:
        mid = (lo+hi)//2
        midval = lst1[mid]
        if midval < k:
            lo = mid +1
        elif midval > k:
            hi = mid
        else:
            return mid
    return -1


def find_time_occurrence_list(seq, k):
    """ find how many times a k element appears in a sorted list.
    One way of doing this is using collections.OrderedDict to no
    mess with the sorting, and add entries for every count. This
    should be O(n). It has a O(1) space complexity since the size of
    the dict is fixed. Another way, since the array is sorted, it to
    use binary search, since this is only O(logn).
    """
    index_some_k = binary_serch_counting(seq, k)
    count = 1
    sizet = len(seq)

    for i in range(index_some_k+1, sizet): # go up
        if seq[i] == k: count +=1
        else: break

    for i in range(index_some_k-1, -1, -1): # go down
        if seq[i] == k: count +=1
        else: break

    return count


def test_find_time_occurrence_list():
    seq = [1,2,2,2,2,2,2,5,6,6,7,8,9]
    k = 2
    assert(find_time_occurrence_list(seq, k) == 6)
    print('Tests passed!')


if __name__ == '__main__':
    test_find_time_occurrence_list()

