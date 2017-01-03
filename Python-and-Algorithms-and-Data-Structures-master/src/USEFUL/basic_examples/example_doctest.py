#!/usr/bin/python

__author__ = "bt3"

'''
The doctest module automatically runs any statement beginning with >>>
and compares the following line with the output from the interpreter.

>>> 1 == 1
False
'''

if __name__ == '__main__':
    import doctest
    doctest.testmod()