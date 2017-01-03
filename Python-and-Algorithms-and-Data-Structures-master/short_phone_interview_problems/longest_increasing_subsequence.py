#!/usr/bin/env python

__author__ = "bt3"


def longest_increasing_subsequence(seq):
    result, aux = [], []
    seq.append(-float('infinity'))
    
    for i, value in enumerate(seq[:-1]):
        aux.append(value)
        if value > seq[i+1]:
            if len(result) < len(aux):
                result = aux[:]
            aux = []
    return result
             
    
    
if __name__ == '__main__':
    seq = [10, -12, 2, 3, -3, 5, -1, 2, -10]
    result = [-12, 2, 3]
    assert(longest_increasing_subsequence(seq) == result)
    
    seq = [2]
    result = [2]
    assert(longest_increasing_subsequence(seq) == result)

    seq = []
    result = []
    assert(longest_increasing_subsequence(seq) == result)