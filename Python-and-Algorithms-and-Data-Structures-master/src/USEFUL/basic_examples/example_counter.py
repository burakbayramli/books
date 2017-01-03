#!/usr/bin/env python

__author__ = "bt3"

from collections import Counter

def  Counter_example():
    ''' it is a dictionary that maps the items to the number of occurrences '''
    seq1 = [1, 2, 3, 5, 1, 2, 5, 5, 2, 5, 1, 4]
    seq_counts = Counter(seq1)
    print(seq_counts)

    ''' we can increment manually or use the update() method '''
    seq2 = [1, 2, 3]
    seq_counts.update(seq2)
    print(seq_counts)

    seq3 = [1, 4, 3]
    for key in seq3:
        seq_counts[key] += 1
    print(seq_counts)

    ''' also, we can use set operations such as a-b or a+b '''
    seq_counts_2 = Counter(seq3)
    print(seq_counts_2)
    print(seq_counts + seq_counts_2)
    print(seq_counts - seq_counts_2)

if __name__ == '__main__':
    Counter_example()


