#!/usr/bin/env python

__author__ = "bt3"

def reverse_str_inplace(_str):
    if len(_str) < 2:
        return _str
    return _str[-1] + reverse_str(_str[1:-1]) + _str[0]
    

def reverse_str(_str):
    result = ''
    j = len(_str) - 1

    while j >= 0:
        result += _str[j]
    
    return result
    
    
if __name__ == '__main__':
    _str = ''
    result = ''
    assert(reverse_str(_str) == result)
    assert(reverse_str_inplace(_str) == result)

    _str = 'a'
    result = 'a'
    assert(reverse_str(_str) == result)
    assert(reverse_str_inplace(_str) == result)

    _str = 'abcde'
    result = 'edcba'
    assert(reverse_str(_str) == result)
    assert(reverse_str_inplace(_str) == result)

    _str = 'abcdef'
    result = 'fedcba'
    assert(reverse_str(_str) == result)
    assert(reverse_str_inplace(_str) == result)