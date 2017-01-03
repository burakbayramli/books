#!/usr/bin/env python

__author__ = "bt3"


''' Given a real number between 0 and 1 (eg: 0.72), this method print the
    binary representation. If the Number cannot be represented accurately
    in binary, with at exit most 32 chars, print error:
'''

def get_float_rep(num):
    if num >= 1 or num <= 0: return 'Error 1'
    result = '.'
    while num:
        if len(result) >= 32: return 'Error 2', result
        r = num*2
        if r >= 1:
            result += '1'
            num = r - 1
        else:
            result += '0'
            num = r
    return result


if __name__ == '__main__':
    print get_float_rep(0.72) #('Error 2', '.1011100001010001111010111000010')
    print get_float_rep(0.1) # ('Error 2', '.0001100110011001100110011001100')
    print get_float_rep(0.5) #'.1'
