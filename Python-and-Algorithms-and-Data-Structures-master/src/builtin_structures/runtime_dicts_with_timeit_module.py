#!/usr/bin/env python

__author__ = "bt3"

'''To use timeit you create a Timer object whose parameters are two Python
statements. The first parameter is a Python statement that you want to time;
the second parameter is a statement that will run once to set up the test.
The timeit module will then time how long it takes to execute the statement
some number of times. By default timeit will try to run the statement one
million times. When its done it returns the time as a floating point value
representing the total number of seconds. However, since it executes the
statement a million times you can read the result as the number of
microseconds to execute the test one time. You can also pass timeit a
named parameter called number that allows you to specify how many times the
test statement is executed. The following session shows how long it takes to
run each of our test functions 1000 times. '''


import timeit
import random

for i in range(10000,1000001,20000):
    ''' this experiment confirm that the contains operator for lists is O(n) and for dict is O(1) '''
    t = timeit.Timer("random.randrange(%d) in x"%i, "from __main__ import random,x")
    x = list(range(i))
    lst_time = t.timeit(number=1000)
    x = {j:None for j in range(i)}
    d_time = t.timeit(number=1000)
    print("%d,%10.3f,%10.3f" % (i, lst_time, d_time))


""" There results are:
10000,     0.192,     0.002
30000,     0.600,     0.002
50000,     1.000,     0.002
70000,     1.348,     0.002
90000,     1.755,     0.002
110000,     2.194,     0.002
130000,     2.635,     0.002
150000,     2.951,     0.002
170000,     3.405,     0.002
190000,     3.743,     0.002
210000,     4.142,     0.002
230000,     4.577,     0.002
250000,     4.797,     0.002
270000,     5.371,     0.002
290000,     5.690,     0.002
310000,     5.977,     0.002

so we can see the linear tile for lists, and constant for dict!

Big-O Efficiency of Python Dictionary Operations
Operation	      Big-O Efficiency
copy	            O(n)
get item	        O(1)
set item	        O(1)
delete item	        O(1)
contains (in)	    O(1)
iteration	    O(n)
"""

