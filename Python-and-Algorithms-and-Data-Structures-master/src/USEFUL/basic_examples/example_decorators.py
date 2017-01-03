#!/usr/bin/env python

__author__ = "bt3"



def logger(func):
    def inner(*args): #1
        print "Arguments were: {0}".format(args)
        return func(*args)
    return inner

@logger
def foo(x, y):
    return x+y

print foo(1, 2)


def sum(func):
    s = 0
    for i in func():
        s += i
    return s

@sum
def interate():
    a = []
    for i in range(10):
        a.append(i)
    return a

print interate

# which is the same as
def interate():
    a = []
    for i in range(10):
        a.append(i)
    return a

print sum(interate)