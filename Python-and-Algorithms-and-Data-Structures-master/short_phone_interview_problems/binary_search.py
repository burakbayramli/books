#!/usr/bin/env python

__author__ = "bt3"

def binary_search(array, value):   
    last, first = len(array), 0
    
    while first < last:
        mid = (last - first)//2
        item = array[mid]
        
        if item == value:
            return True
        
        elif item < value:
            last = mid
        
        else:
            first = mid 
    
    return False

def binary_search_rec(array, value, first=0, last=None):
    last = last or len(array)
    if len(array[first:last]) < 1:
        return False
    
    mid = (last - first)//2
    if array[mid] == value:
        return True
    elif array[mid] < value:
        return binary_search_rec(array, value, first=first, last=mid)
    else:
        return binary_search_rec(array, value, first=mid, last=last)

    
if __name__ == '__main__':    
    array = [3, 4, 6, 7, 10, 11, 34, 67, 84]
    value = 6
    assert(binary_search(array, value) == True)   
    assert(binary_search_rec(array, value) == True)  
    value = 8
    assert(binary_search(array, value) == False)
    assert(binary_search_rec(array, value) == False)  
    array = [8]
    assert(binary_search(array, value) == True)
    assert(binary_search_rec(array, value) == True)  
    array = []
    assert(binary_search(array, value) == False)
    assert(binary_search_rec(array, value) == False)  