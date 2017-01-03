#!/usr/bin/env python

__author__ = "bt3"

''' implement square root using binary search '''


def find_sqrt_bin_search(n, error=0.001):
    lower = n < 1 and n or 1
    upper = n < 1 and 1 or n
    mid = lower + (upper - lower) / 2.0
    square = mid * mid
    while abs(square - n) > error:
        if square < n:
            lower = mid
        else:
            upper = mid
        mid = lower + (upper - lower) / 2.0
        square = mid * mid
    return mid


def test_ind_sqrt_bin_search():
    number = 9
    assert(find_sqrt_bin_search(number) == 3)
    print('Tests passed!')


if __name__ == '__main__':
    test_ind_sqrt_bin_search()





