#!/usr/bin/env python

__author__ = "bt3"

from collections import Counter

def find_top_N_recurring_words(seq, N):
    ''' find the top N recurring words in a file:
        1) use a hash table to find counts
        2) sort the list on base of the maximum counts
        3) return the last N words
    '''

    dcounter = Counter()
    for word in seq.split():
        dcounter[word] += 1

    return dcounter.most_common(N)



if __name__ == '__main__':
    seq = 'buffy angel monster xander a willow gg buffy the monster super buffy angel'
    N = 3
    assert(find_top_N_recurring_words(seq, N) == [('buffy', 3), ('monster', 2), ('angel', 2)])


