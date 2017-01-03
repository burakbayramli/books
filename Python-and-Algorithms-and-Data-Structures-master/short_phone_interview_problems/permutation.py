#!/usr/bin/env python

__author__ = "bt3"


def permutation(array):
    if len(array) < 2:
        return [array]
    
    result = []
    for index, letter in enumerate(array):
        new_array = array[:index] + array[index+1:]
        for perm in permutation(new_array):
            result.append(letter + perm)
    
    return result
    
    
    
if __name__ == '__main__':
    word = 'abc'
    result = ['abc', 'acb', 'bac', 'bca', 'cab', 'cba']
    assert(permutation(word) == result)
    
    word = ''
    result = ['']
    assert(permutation(word) == result)
    
    word = 'a'
    result = ['a']
    assert(permutation(word) == result)