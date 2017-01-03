#!/usr/bin/env python

__author__ = "bt3"



from collections import defaultdict

def is_palindrome(array):
    '''
    >>> is_palindrome('subi no onibus')
    True
    >>> is_palindrome('helllo there')
    False
    >>> is_palindrome('h')
    True
    >>> is_palindrome('')
    True
    '''
    array = array.strip(' ')
    if len(array) < 2:
        return True

    if array[0] == array[-1]:
        return is_palindrome(array[1:-1])
    else:
        return False


if __name__ == '__main__':
    import doctest
    doctest.testmod()

