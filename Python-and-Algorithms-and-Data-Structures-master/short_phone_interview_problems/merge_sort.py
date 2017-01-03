#!/usr/bin/env python
# AKA: do you believe in magic?

__author__ = "bt3"

def merge_sort(array):
    if len(array) < 2:
        return array
    
    # divide
    mid = len(array)//2
    left = merge_sort(array[:mid])
    right = merge_sort(array[mid:])
    
    # merge
    result = []
    i, j = 0, 0
    
    while i < len(left) and j < len(right):
        if left[i] < right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j+= 1
    
    # make sure nothing is left behind
    if left[i:]:
        result.extend(left[i:])
    if right[j:]:
        result.extend(right[j:])
    
    return result
    
    
    
    
if __name__ == '__main__':
    array = [3, 1, 6, 0, 7, 19, 7, 2, 22]
    sorted = [0, 1, 2, 3, 6, 7, 7, 19, 22]
    assert(merge_sort(array) == sorted)
    
    array = []
    assert(merge_sort(array) == array)

    array = [1]
    assert(merge_sort(array) == array)