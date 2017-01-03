#!/usr/bin/env python

__author__ = "bt3"

def rotate_NxN(m):
    n = len(m)
    for layer in range(n//2):
        first = layer
        last = n - 1 - layer
        for i in range(first, last):
            offset = i - first
            top = m[first][i]
            m[first][i] = m[last-offset][first]
            m[last-offset][first] = m[last][last-offset]
            m[last][last-offset] = m[i][last]
            m[i][last] = top
    return m



def main():
    m = [[1,2],[3,4]]
    mr = [[3,1],[4,2]]
    assert(rotate_NxN(m) == mr)
    m2 = [[1,2,3],[4,5,6],[7,8,9]]
    print(rotate_NxN(m2))

if __name__ == '__main__':
    main()

