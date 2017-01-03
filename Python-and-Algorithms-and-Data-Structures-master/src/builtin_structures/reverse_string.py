#!/usr/bin/env python

__author__ = "bt3"


def revert(string):
    '''
    >>> s = 'hello'
    >>> revert(s)
    'olleh'
    >>> revert('')
    ''
    '''
    return string[::-1]



def reverse_string_inplace(s):
    '''
    >>> s = 'hello'
    >>> reverse_string_inplace(s)
    'olleh'
    >>> reverse_string_inplace('')
    ''
    '''
    if s:
        s = s[-1] + reverse_string_inplace(s[:-1])
    return s






if __name__ == '__main__':
    import doctest
    doctest.testmod()