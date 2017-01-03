#!/usr/bin/env python

__author__ = "bt3"

''' This method merges set bit and clean bit:
    1) first clear the bit at i using a mask such as 1110111
    2) then shift the intended value v by i bits
    3) this will create a number with bit i to v and all other to 0
    4) finally update the ith bit with or
'''



def  update_bit(num, i, v):
    mask = ~ (1 << i)
    return bin( (num & mask) | (v << i) )


if __name__ == '__main__':
    num = int('10010000', 2)
    print update_bit(num, 2, 1)   # '0b10010100'

