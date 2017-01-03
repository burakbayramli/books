
#!/usr/bin/env python

__author__ = "bt3"

import time
import numpy as np

def testing_numpy():
    ''' tests many features of numpy '''
    ax = np.array([1,2,3])
    ay = np.array([3,4,5])
    print(ax)
    print(ax*2)
    print(ax+10)
    print(np.sqrt(ax))
    print(np.cos(ax))
    print(ax-ay)
    print(np.where(ax<2, ax, 10))

    m = np.matrix([ax, ay, ax])
    print(m)
    print(m.T)

    grid1 = np.zeros(shape=(10,10), dtype=float)
    grid2 = np.ones(shape=(10,10), dtype=float)
    print(grid1)
    print(grid2)
    print(grid1[1]+10)
    print(grid2[:,2]*2)


def trad_version():
    t1 = time.time()
    X = range(10000000)
    Y = range(10000000)
    Z = []
    for i in range(len(X)):
        Z.append(X[i] + Y[i])
    return time.time() - t1

def numpy_version():
    t1 = time.time()
    X = np.arange(10000000)
    Y = np.arange(10000000)
    Z = X + Y
    return time.time() - t1



if __name__ == '__main__':
    testing_numpy()
    print(trad_version())
    print(numpy_version())


'''
3.23564291
0.0714290142059
'''
