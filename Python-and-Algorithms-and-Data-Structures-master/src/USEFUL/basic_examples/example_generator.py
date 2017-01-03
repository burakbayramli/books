#!/usr/bin/env python

__author__ = "bt3"


def interate(x):
    for i in range(x):
        yield i

def gen1():
    a =  interate(10)
    print a.next()
    print a.next()
    print a.next()


def reverse(data):
    for i in range(len(data)-1, -1, -1):
        yield data[i]

def gen2():
    for c in reverse('awesome'):
        print c

if __name__ == '__main__':
    gen1()
    gen2()
