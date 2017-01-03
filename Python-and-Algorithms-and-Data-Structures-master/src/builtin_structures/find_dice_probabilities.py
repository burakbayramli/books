#!/usr/bin/env python

__author__ = "bt3"

'''
given 2 dice, determine number of ways to sum S if all dice are rolled
'''

from collections import Counter, defaultdict

def find_dice_probabilities(S=5, n_faces=6):
    if S > 2*n_faces or S < 2:
        return None

    cdict = Counter()
    ddict = defaultdict(list)

    for dice1 in range(1, n_faces+1):
        for dice2 in range(1, n_faces+1):
            t = [dice1, dice2]
            cdict[dice1+dice2] += 1
            ddict[dice1+dice2].append( t)

    return [cdict[S], ddict[S]]





if __name__ == '__main__':
    results = find_dice_probabilities()
    assert(results[0] == len(results[1]))
