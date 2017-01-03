#!/usr/bin/env python

__author__ = "bt3"


""" Using our deque class and Python's deque class """


import string
import collections

from deque import Deque


STRIP = string.whitespace + string.punctuation + "\"'"

def palindrome_checker_with_deque(str1):

    d1 = Deque()
    d2 = collections.deque()

    for s in str1.lower():
        if s not in STRIP:
            d2.append(s)
            d1.enqueue(s)


    eq1 = True
    while d1.size() > 1 and eq1:
        if d1.dequeue_front() != d1.dequeue():
            eq1 = False

    eq2 = True
    while len(d2) > 1 and eq2:
        if d2.pop() != d2.popleft():
            eq2 = False

    return eq1, eq2




if __name__ == '__main__':
    str1 = 'Madam Im Adam'
    str2 = 'Buffy is a Slayer'
    print(palindrome_checker_with_deque(str1))
    print(palindrome_checker_with_deque(str2))