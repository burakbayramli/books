
# Code from Chapter 12 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# A greedy algorithm to solve the Knapsack problem
from numpy import *

def greedy():
    maxSize = 500    
    sizes = array([109.60,125.48,52.16,195.55,58.67,61.87,92.95,93.14,155.05,110.89,13.34,132.49,194.03,121.29,179.33,139.02,198.78,192.57,81.66,128.90])

    sizes.sort()
    newSizes = sizes[-1:0:-1]
    space = maxSize
    
    while len(newSizes)>0 and space>newSizes[-1]:
        # Pick largest item that will fit
        item = where(space>newSizes)[0][0]
        print newSizes[item]
        space = space-newSizes[item]
        newSizes = concatenate((newSizes[:item],newSizes[item+1:]))
    print "Size = ",maxSize-space
    
greedy() 
