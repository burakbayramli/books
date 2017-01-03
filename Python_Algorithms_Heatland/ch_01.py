# Interactive code for Chapter 1

# The timing code is not included in the book:
"""
>>> from time import *
>>> t0 = time()
>>> count = 10**4 # Change to 10**5 for original (slow) test
>>> nums = []
>>> for i in range(count):
...     nums.append(i)
... 
>>> nums.reverse()
>>> t1 = time() - t0
>>> t0 = time()
>>> nums = []
>>> for i in range(count):
...     nums.insert(0, i)
... 
>>> t2 = time() - t0
"""
