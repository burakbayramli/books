#!/usr/bin/env python

__author__ = "bt3"


''' Give a positive int, print the next smallest and next largest ints with
    same number of 1 bits.
    The brute force is:
    1) find number of 1 bits
    2) loop above and down until find same, checking for each
'''



def print_prev_same_1s(num):
    n1s = find_num_1s(num)
    # find prev
    i = num-1
    while True:
        n1s_here = find_num_1s(i)
        if n1s_here == n1s:
            return bin(i)
        i -= 1
        if i < 0:
            return None

def print_next_same_1s(num):
    n1s = find_num_1s(num)
    # find next
    i = num+1
    while True:
        n1s_here = find_num_1s(i)
        if n1s_here == n1s:
            return bin(i)
        i += 1
        if i < 0:
            return None



def find_num_1s(num):
    counter = 0
    while num:
        if num & 1:
            counter += 1
        num >>= 1
    return counter





if __name__ == '__main__':
    num = 0b1001
    n = '0b1010'
    p = '0b110'
    print_prev_same_1s(num) == p
    print_next_same_1s(num) == n

