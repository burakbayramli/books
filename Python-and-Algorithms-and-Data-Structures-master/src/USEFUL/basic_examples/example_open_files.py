#!/usr/bin/env python

__author__ = "bt3"

filename = raw_input('Enter a file name: ')
try:
    f = open(filename, "r")
except:
    print 'There is no file named', filename

