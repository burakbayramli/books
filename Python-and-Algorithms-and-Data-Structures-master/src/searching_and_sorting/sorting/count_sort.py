#!/usr/bin/env python

__author__ = "bt3"



from collections import defaultdict

def count_sort_dict(a):
    ''' an example of counting sort using default dictionaries '''
    b, c = [], defaultdict(list)
    for x in a:
        c[x].append(x) # we could have used key = lambda x:x
    for k in range(min(c), max(c) + 1):
        b.extend(c[k])
    return b


def test_count_sort():
    seq = [3, 5, 2, 6, 8, 1, 0, 3, 5, 6, 2, 5, 4, 1, 5, 3]
    assert(count_sort_dict(seq) == sorted(seq))


if __name__ == '__main__':
    test_count_sort()


