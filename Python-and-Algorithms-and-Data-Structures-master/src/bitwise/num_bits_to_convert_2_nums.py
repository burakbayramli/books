#!/usr/bin/env python

__author__ = "bt3"


''' This method returns the number of bits that are necessary to change to convert two
    numbers A and B:
    1) XOR
    2) count 1s
'''

def count_bits_swap2(a, b):
    count = 0
    m = a^b
    while m:
        count +=1
        m = m & (m-1)
    return count



def count_bits_swap(a, b):
    m = a^b
    return count_1s(m)


def count_1s(m):
    count = 0
    while m:
        if m& 1 :
            count +=1
        m >>= 1
    return count


if __name__ == '__main__':
    a = int('10010000', 2)
    b = int('01011010', 2)
    print count_bits_swap(a, b)  #4
    print count_bits_swap2(a, b)  #4
