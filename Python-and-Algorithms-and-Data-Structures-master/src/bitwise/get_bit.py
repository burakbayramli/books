#!/usr/bin/env python

__author__ = "bt3"

''' Get a bit in a binary number:
    1) Shifts 1 over by i bits
    2) make an AND with the number
    3) all the other than the bit at i are clean, now compare to 0
    4) if the new value is not 0, bit i is 1
'''


def get_bit(num, i):
    mask = 1 << i
    return num & mask != 0


if __name__ == '__main__':
    num = int('0100100', 2)
    get_bit(num, 0)   # 0
    get_bit(num, 1)   # 0
    get_bit(num, 2)   # 1
    get_bit(num, 3)   # 0
    get_bit(num, 4)   # 0
    get_bit(num, 5)   # 1
    get_bit(num, 6)   # 0

