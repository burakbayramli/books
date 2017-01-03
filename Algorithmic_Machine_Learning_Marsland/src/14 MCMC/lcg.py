
# Code from Chapter 14 of Machine Learning: An Algorithmic Perspective
# by Stephen Marsland (http://seat.massey.ac.nz/personal/s.r.marsland/MLBook.html)

# You are free to use, change, or redistribute the code in any way you wish for
# non-commercial purposes, but please maintain the name of the original author.
# This code comes with no warranty of any kind.

# Stephen Marsland, 2008

# The linear congruential pseudo-random number generator
from numpy import *

def lcg(x0,n):
    # These choices show the periodicity very well
    # Better choices are a = 16,807 m = 2**31 -1 c = 0
    # Or m = 2**32 a = 1,664,525 c = 1,013,904,223
    a = 23
    m = 197
    c = 0
    
    rnd = zeros((n))
    
    rnd[0] = mod(a*x0 + c,m)
    
    for i in range(1,n):
        rnd[i] = mod(a*rnd[i-1]+c,m)
        
    return rnd
    
print lcg(3,80) 
