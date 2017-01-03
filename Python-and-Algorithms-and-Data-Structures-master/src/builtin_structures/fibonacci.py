#!/usr/bin/env python

__author__ = "bt3"


def fib_generator():
    a, b = 0, 1

    while True:
        yield b
        a, b = b, a+b


def fib(n):
    '''
    >>> fib(2)
    1
    >>> fib(5)
    5
    >>> fib(7)
    13
    '''
    if n < 3:
        return 1

    a, b = 0, 1
    count = 1

    while count < n:
        count += 1
        a, b = b, a+b

    return b


def fib_rec(n):
    '''
    >>> fib_rec(2)
    1
    >>> fib_rec(5)
    5
    >>> fib_rec(7)
    13
    '''
    if n < 3:
        return 1
    return fib_rec(n - 1) + fib_rec(n - 2)




if __name__ == '__main__':
    import doctest
    doctest.testmod()

    fib = fib_generator()
    print(next(fib))
    print(next(fib))
    print(next(fib))
    print(next(fib))
