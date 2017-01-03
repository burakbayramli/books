#!/usr/bin/env python
from ctypes import *
hw_lib = CDLL('hw.so')  # load shared library

hw_lib.hw1.restype = c_double  # specify return type
s = hw_lib.hw1(c_double(1), c_double(2.14159))
print s, type(s)

# automatic conversion of arguments from Python to ctypes:
hw_lib.hw1.argtypes = [c_double, c_double]
s = hw_lib.hw1(1, 2.14159)
print s, type(s)

hw_lib.hw2.argtypes = [c_double, c_double]
hw_lib.hw1.restype = None  # returns void
hw_lib.hw2(1, 2.14159)

s = c_double()
hw_lib.hw3(c_double(1), c_double(2.14159), byref(s))
print s.value



