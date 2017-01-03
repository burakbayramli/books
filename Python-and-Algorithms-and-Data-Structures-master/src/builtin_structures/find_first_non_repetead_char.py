#!/usr/bin/env python

__author__ = "bt3"

from collections import Counter

def  find_non_rep_char(s1):
  '''
  >>> s1 = 'aabbcceff'
  >>> find_non_rep_char(s1)
  e
  >>> find_non_rep_char('ccc')
  '''

  aux_dict = Counter()

  for i in s1:
      aux_dict[i] += 1

  for k, v in aux_dict.items():
    if v < 2:
        print k



if __name__ == '__main__':
    import doctest
    doctest.testmod()

