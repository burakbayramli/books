#!/usr/bin/env python

__author__ = "bt3"


def longest_common_substring(s1, s2):
    p1 = 0
    aux, lcp = '', ''
    string1 = max(s1, s2)
    string2 = min(s1, s2)

    while p1 < len(string1):
        p2 = 0
        while  p2 < len(string2) and p1+p2 < len(string1):
            if string1[p1+p2] == string2[p2]:
                aux += string1[p1+p2]
            else:
                if len(lcp) < len(aux):
                    lcp = aux
                aux = ''
            p2 += 1
        p1 += 1

    return lcp
             
    
    
if __name__ == '__main__':
    str1 = 'hasfgeaae'
    str2 = 'bafgekk'
    result = 'fge'
    assert(longest_common_substring(str1, str2) == result)