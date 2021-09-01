# Ascending sort of an array
from random import *
from sort import *
from graphlib import *

# main

n = 50;                                       # number of values to be sorted
x = [0]*(n+1); x0 = [0]*(n+1)                   # array to be sorted and copy
ix = [0]*(n+1)                                  # array of sequential indexes
ind = [0]*(n+1)                                            # array of indexes

for i in range(1,n+1):
   x[i] = x0[i] = random()                               # array to be sorted
   ix[i] = float(i)

GraphInit(1200,600)

Plot(ix,x,n,"blue",3,0.06,0.32,0.25,0.80,"i","x","Initial array")

InsertSort(x,n)

Plot(ix,x,n,"blue",3,0.39,0.65,0.25,0.80,"i","x","Sorted array")

Index(x0,ind,n)
for i in range(1,n+1): x[i] = x0[ind[i]]                   # sort by indexing

Plot(ix,x,n,"blue",3,0.72,0.98,0.25,0.80,"i","x","Indexed array")

MainLoop()
