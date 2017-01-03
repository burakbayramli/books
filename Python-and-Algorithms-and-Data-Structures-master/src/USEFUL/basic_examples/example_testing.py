#!/usr/bin/env python

__author__ = "bt3"



def test_doctest():
    '''
    >>> 1 == 1
    False
    '''
    pass

if __name__ == '__main__':
    import doctest
    doctest.testmod()

#####

import unittest

class BasicsTestCase(unittest.TestCase):

    def test_find_name(self):
        self.assertTrue(1 == 1)
        self.assertFalse(1 == 2)

if __name__ == '__main__':
    unittest.main()



#####

# content of test_example.py, run with $ py.test
#
# run tests over the directory
# $ nosetest


def func(x):
    return x + 1

def test_answer():
    assert func(3) == 4

