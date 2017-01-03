#!/usr/bin/env python

__author__ = "bt3"



''' Set a bit in a binary number:
    1) Shifts 1 over by i bits
    2) make an OR with the number, only the value at bit i will change and all the others bit
    of the mask are zero so will not affect the num
'''


def set_bit(num, i):
    mask = 1 << i
    return bin( num | mask )


if __name__ == '__main__':
    num = int('0100100', 2)
    print set_bit(num, 0)   #'0b100101'
    print set_bit(num, 1)   #'0b100110'
    print set_bit(num, 2)   # nothing change '0b100100'
    print set_bit(num, 3)   #'0b101100'
    print set_bit(num, 4) #'0b110100'
    print set_bit(num, 5)     # nothing change '0b100100'
