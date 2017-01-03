#!/usr/bin/env python

__author__ = "bt3"

'''To use timeit you create a Timer object whose parameters are two Python statements. The first parameter is a Python statement that you want to time; the second parameter is a statement that will run once to set up the test. The timeit module will then time how long it takes to execute the statement some number of times. By default timeit will try to run the statement one million times. When its done it returns the time as a floating point value representing the total number of seconds. However, since it executes the statement a million times you can read the result as the number of microseconds to execute the test one time. You can also pass timeit a named parameter called number that allows you to specify how many times the test statement is executed. The following session shows how long it takes to run each of our test functions 1000 times. '''


def test1():
    l = []
    for i in range(1000):
        l = l + [i]

def test2():
    l = []
    for i in range(1000):
        l.append(i)

def test3():
    l = [i for i in range(1000)]

def test4():
    l = list(range(1000))



if __name__ == '__main__':
    import timeit
    t1 = timeit.Timer("test1()", "from __main__ import test1")
    print("concat ",t1.timeit(number=1000), "milliseconds")
    t2 = timeit.Timer("test2()", "from __main__ import test2")
    print("append ",t2.timeit(number=1000), "milliseconds")
    t3 = timeit.Timer("test3()", "from __main__ import test3")
    print("comprehension ",t3.timeit(number=1000), "milliseconds")
    t4 = timeit.Timer("test4()", "from __main__ import test4")
    print("list range ",t4.timeit(number=1000), "milliseconds")



""" The results are:
('concat ', 2.366791009902954, 'milliseconds')
('append ', 0.16743111610412598, 'milliseconds')
('comprehension ', 0.06446194648742676, 'milliseconds')
('list range ', 0.021029949188232422, 'milliseconds')


So we see the following pattern for lists:

Operation	      Big-O Efficiency
index []	        O(1)
index assignment	O(1)
append	            O(1)
pop()	            O(1)
pop(i)	            O(n)
insert(i,item)	    O(n)
del operator	    O(n)
iteration	        O(n)
contains (in)	    O(n)
get slice [x:y]	    O(k)
del slice	        O(n)
set slice	        O(n+k)
reverse	            O(n)
concatenate	        O(k)
sort	            O(n log n)
multiply	        O(nk)
"""

