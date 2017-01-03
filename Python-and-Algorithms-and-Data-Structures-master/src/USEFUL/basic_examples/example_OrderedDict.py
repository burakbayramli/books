#!/usr/bin/env python

__author__ = "bt3"

from collections import OrderedDict

def  OrderedDict_example():
    ''' show some examples for OrderedDict '''
    ''' keep the order of insertion.
        maintains a doubly linked list, so size is more than twice than normal dict'''


    pairs = [('a', 1), ('b',2), ('c',3)]

    d1 = {}
    for key, value in pairs:
        if key not in d1:
            d1[key] = []
        d1[key].append(value)
    for key in d1:
        print(key, d1[key])

    d2 = OrderedDict(pairs)
    for key in d2:
        print(key, d2[key])


if __name__ == '__main__':
    OrderedDict_example()


