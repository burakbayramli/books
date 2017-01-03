#!/usr/bin/env python

__author__ = "bt3"


import random

def benchmark(func):
    import time
    def wrapper(*args, **kwargs):
        t = time.clock()
        res = func(*args, **kwargs)
        print("\t%s" % func.__name__, time.clock()-t)
        return res
    return wrapper


@benchmark
def random_tree(n):
    temp = [n for n in range(n)]
    for i in range(n+1):
        temp[random.choice(temp)] = random.choice(temp)
    return temp


if __name__ == '__main__':
    random_tree(10000)

