#!/usr/bin/env python3

__author__ = "bt3"


from functools import lru_cache


@lru_cache(maxsize=20)
def fib(n):
    if n < 2:
        return n
    return fib(n-1) + fib(n-2)


if __name__ == '__main__':
    print([fib(n) for n in range(10)])
    print(fib.cache_info())
