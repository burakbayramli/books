#!/usr/bin/env python

__author__ = "bt3"



def reversing_words(word):
    """
    >>> reversing_words('buffy is awesome')
    'awesome is buffy'
    """
    new_word = []

    words = word.split(' ')
    for word in words[::-1]:
        new_word.append(word)

    return " ".join(new_word)


def reversing_words2(s):
    """
    >>> reversing_words2('buffy is awesome')
    'awesome is buffy'
    """
    words = s.split()
    return ' '.join(reversed(words))


def reversing_words3(s):
    """
    >>> reversing_words('buffy is awesome')
    'awesome is buffy'
    """
    words = s.split(' ')
    words.reverse()
    return ' '.join(words)




if __name__ == '__main__':
    import doctest
    doctest.testmod()

