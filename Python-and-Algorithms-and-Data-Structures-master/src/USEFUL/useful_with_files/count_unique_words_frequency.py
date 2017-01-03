#!/usr/bin/python

__author__ = "bt3"


import collections
import sys

def count_unique_word_freq():
        return collections.Counter(\
            sys.stdin.read().lower().split()).most_common(n)


if __name__ == '__main__':
    count_unique_word_freq()