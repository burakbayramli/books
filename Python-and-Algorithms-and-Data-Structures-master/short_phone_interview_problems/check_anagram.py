#!/usr/bin/env python

__author__ = "bt3"

from collections import Counter

def check_if_anagram(word1, word2):
    counter = Counter()
    
    for c in word1:
        counter[c] += 1
    
    for c in word2:
        counter[c] -= 1
        
    for values in counter.values():
        if values != 0:
            return False

    return True
    
    
    
if __name__ == '__main__':
    word1 = 'abc'
    word2 = 'bca'
    assert(check_if_anagram(word1, word2) == True)
    
    word2 = 'bcd'
    assert(check_if_anagram(word1, word2) == False)
    
    word1 = ''
    word2 = ''
    assert(check_if_anagram(word1, word2) == True)
    
    word1 = 'a'
    word2 = 'a'
    assert(check_if_anagram(word1, word2) == True)