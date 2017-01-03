#!/usr/bin/env python

__author__ = "bt3"


'''
Implement a trie. (Write the API and code for inserting into a trie).
'''

def make_trie(*args):
    trie = {}

    for word in args:
        temp_trie = trie
        for letter in word:
            temp_trie = temp_trie.setdefault(letter, {})
        temp_trie = temp_trie.setdefault('_end_', '_end_')

    return trie


def in_trie(trie, word):
    temp_trie = trie
    for letter in word:
        if letter not in temp_trie:
            return False
        temp_trie = temp_trie[letter]
    return True


def remove_from_trie(trie, word, depth):
    if word and word[depth] not in trie:
        return False

    if len(word) == depth + 1:
        del trie[word[depth]]
        if not trie:
           return True
        return False
    else:
        temp_trie = trie

        if remove_from_trie(temp_trie[word[depth]], word, depth + 1):
            if temp_trie:
                del temp_trie[word[depth]]
            return not temp_trie
    return False


if __name__ == '__main__':
    trie = make_trie('hello', 'abc', 'baz', 'bar', 'barz')
    print 'This is the trie:'
    print trie

    assert(in_trie(trie, 'hello') == True)
    assert(in_trie(trie, 'bar') == True)
    assert(in_trie(trie, 'bab') == False)
    assert(in_trie(trie, 'zzz') == False)

    remove_from_trie(trie, 'abc', 0)
    assert(in_trie(trie, 'abc') == False)




