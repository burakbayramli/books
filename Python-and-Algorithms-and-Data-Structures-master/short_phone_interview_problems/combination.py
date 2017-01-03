#!/usr/bin/env python

__author__ = "bt3"

def combination(array):
    if len(array) < 2:
        return set(array)
    
    result = set()
    for index, item in enumerate(array):
        new_array = array[:index] + array[index+1:]
        result.add(item)
        for perm in combination(new_array):
            new_item = ''.join(sorted(item + perm))
            result.add(new_item)
    
    return result
    
    
    
if __name__ == '__main__':
    array = ['a', 'b', 'c']
    result = set(['a', 'ac', 'ab', 'abc', 'bc', 'c', 'b']) 
    assert(combination(array) == result)
    
    array = ['']
    result = set([''])
    assert(combination(array) == result)
    
    array = ['a']
    result = set(['a'])
    assert(combination(array) == result)