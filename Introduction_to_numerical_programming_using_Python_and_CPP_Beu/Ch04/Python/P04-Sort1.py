# Ascending sort and indexing of an array
from sort import *

# main

n = 6                                         # number of values to be sorted
x = [0., 30., 60., 50., 20., 10., 40.]                   # array to be sorted
ind = [0]*(n+1); rnk = [0]*(n+1)                # arrays of indexes and ranks

print("Original array:")
for i in range(1,n+1): print("{0:6.2f}".format(x[i]),end="")
print()

Index(x,ind,n)
print("Indexes:")
for i in range(1,n+1): print("{0:6d}".format(ind[i]),end="")
print()

Rank(ind,rnk,n)
print("Ranks:")
for i in range(1,n+1): print("{0:6d}".format(rnk[i]),end="")
print()

print("Indexed array:")
for i in range(1,n+1): print("{0:6.2f}".format(x[ind[i]]),end="")
print()

InsertSort(x,n)
print("Sorted array:")
for i in range(1,n+1): print("{0:6.2f}".format(x[i]),end="")
print()
