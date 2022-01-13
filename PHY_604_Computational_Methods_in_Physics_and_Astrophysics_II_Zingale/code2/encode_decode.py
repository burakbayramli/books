from __future__ import print_function

import numpy as np

def encode(r, m):
    """ r is the number to be encoded, m is the number of bits 
        we assume that 0 < r < 1 """

    y = np.zeros(m, dtype=np.bool)

    y[0] = bool(round(r))
    for j in range(1,m):
        tmp = np.sum(2**(j-np.arange(j)-1)*y[:j])
        y[j] = bool(round(2**j*r - tmp))

    return y

def decode(y):
    r = np.sum(y/2.0**(np.arange(len(y))+1))
    return r



if __name__ == "__main__":

    nbits = [4, 6, 8, 10, 15, 20]

    rs = np.random.random(size=10)

    print("{:12}".format("r"), end=" ")

    for n in nbits:
        print("{:4}{:8}".format(n, " bits"), end=" ")

    print("")

    for r in rs:
        print("{:12.10f} |".format(r), end=" ")
        for n in nbits:
            y = encode(r, n)
            print("{:12.10f}".format(decode(y)), end=" ")
        print("")




