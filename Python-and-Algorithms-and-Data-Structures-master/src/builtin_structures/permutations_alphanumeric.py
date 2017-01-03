#!/usr/bin/env python

__author__ = "bt3"


''' Write code to generate all possible case permutations of a given
lower-cased string
'''

def alpha_permutation(string):
    '''
    >>> alpha_permutation('0ab')
    ['0Ab', '0Ab', '0ab', '0ab', '0Ba', '0Ba', '0ba', '0ba', 'ab0', 'a0b', 'a0b', 'b0a', 'b0a', 'ba0']
    >>> alpha_permutation('')
    ''
    '''

    if len(string) < 2:
        return string

    result = []

    for i, c in enumerate(string):
        rest =  string[i+1:] + string[:i]
        for cc in alpha_permutation(rest):
            if cc.isalpha():
                result.append(c.upper() + cc)
            result.append(c + cc)

    return result



if __name__ == '__main__':
    import doctest
    doctest.testmod()

