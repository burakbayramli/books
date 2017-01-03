#!/usr/bin/env python

__author__ = "bt3"

def quick_sort(array):
    if len(array) < 2:
        return array

    # partition
    ipivot = len(array)//2
    pivot = array[ipivot]
    new_array = array[:ipivot] + array[ipivot+1:]
    
    left = [x for x in new_array if x <= pivot]
    right = [x for x in new_array if x > pivot]

    return quick_sort(left) + [pivot] + quick_sort(right)
    
    
    
    
if __name__ == '__main__':
    array = [3, 1, 6, 0, 7, 19, 7, 2, 22]
    sorted = [0, 1, 2, 3, 6, 7, 7, 19, 22]
    assert(quick_sort(array) == sorted)
    
    array = []
    assert(quick_sort(array) == array)

    array = [1]
    assert(quick_sort(array) == array)