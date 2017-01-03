#!/usr/bin/env python

__author__ = "bt3"


'''
find the sum of two integers represented as strings,
return the sum as string, i.e "123" and "456" would return "579"
'''


def get_number(s):
    count = 1
    num = 0
    p = len(s) -1
    while p >= 0:
        num = num + int(s[p])*count
        count *= 10
        p -= 1
    return num


def sum_string(s1, s2):
    '''
    >>> sum_string('10', '5')
    '15'
    >>> sum_string('0', '1')
    '1'
    >>> sum_string('123', '456')
    '579'
    '''

    n1 = get_number(s1)
    n2 = get_number(s2)
    return str(n2 + n1)


if __name__ == '__main__':
    import doctest
    doctest.testmod()

