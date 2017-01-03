#!/usr/bin/env python

__author__ = "bt3"

''' Clear a bit in a binary number.
    Like the reverse of set bit:
    1) first create a number filled of 1s,
        with 0 at i (can create 0001000 and ~)
    2) AND the number so it clears the ith bit
'''



def clear_bit(num, i):
    mask = ~ (1 << i)   # -0b10001
    return bin(num & mask)


def clear_all_bits_from_i_to_0(num, i):
    mask = ~ ( (1 << (i+1)) - 1)
    return bin(num & mask)


def clear_all_bits_from_most_sig_to_1(num, i):
    mask =  ( 1 << i) -1
    return bin(num & mask)


if __name__ == '__main__':
    num = int('10010000', 2)
    print clear_bit(num, 4)   # '0b10000000'

    num = int('10010011', 2)
    print clear_all_bits_from_i_to_0(num, 2)  # '0b10010000'

    num = int('1110011', 2)
    print clear_all_bits_from_most_sig_to_1(num, 2)  #'0b11'
