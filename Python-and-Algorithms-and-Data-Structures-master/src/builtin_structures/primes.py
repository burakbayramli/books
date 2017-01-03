#!/usr/bin/env python

__author__ = "bt3"


'''
find prime factors of a number.
'''

import math
import random

def find_prime_factors(n):
    '''
    >>> find_prime_factors(14)
    [2, 7]
    >>> find_prime_factors(19)
    []
    '''

    divisors = [d for d in range(2, n//2 + 1) if n % d == 0]
    primes = [d for d in divisors if is_prime(d)]

    return primes


def is_prime(n):
    for j in range(2, int(math.sqrt(n))):
        if (n % j) == 0:
            return False
    return True


''' return a n-bit prime '''
def generate_prime(number=3):
    while 1:
        p = random.randint(pow(2, number-2), pow(2, number-1)-1)
        p = 2 * p + 1
        if find_prime_factors(p):
            return p



if __name__ == '__main__':
    import doctest
    doctest.testmod()

