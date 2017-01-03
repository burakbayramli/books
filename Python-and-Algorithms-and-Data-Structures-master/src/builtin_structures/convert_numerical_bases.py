#!/usr/bin/env python

__author__ = "bt3"

''' convert an integer to a string in any base'''

def convert_from_dec_to_any_base(number, base):
    '''
    >>> number, base = 9, 2
    >>> convert_from_dec_to_any_base(number, base)
    '1001'
    '''

    convertString = '012345679ABCDEF'

    if number < base:
        return convertString[number]

    else:
        return convert_from_dec_to_any_base(number//base, base) + \
                                            convertString[number%base]



def convert_from_decimal_to_binary(number, base):
    '''
    >>> number, base = 9, 2
    >>> convert_from_decimal_to_binary(number, base)
    1001
    '''

    multiplier, result = 1, 0

    while number > 0:
        result += number%base*multiplier
        multiplier *= 10
        number = number//base

    return result



def convert_from_decimal_larger_bases(number, base):
    '''
    >>> number, base = 31, 16
    >>> convert_from_decimal_larger_bases(number, base)
    '1F'
    '''
    strings = "0123456789ABCDEFGHIJ"
    result = ""

    while number > 0:
        digit = number%base
        result = strings[digit] + result
        number = number//base

    return result



if __name__ == '__main__':
    import doctest
    doctest.testmod()

