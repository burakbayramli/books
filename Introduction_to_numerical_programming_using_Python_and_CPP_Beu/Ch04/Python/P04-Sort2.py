# Ascending indexing of two correlated arrays
from sort import *

# main

n = 6                                         # number of values to be sorted
x = [0., 40., 60., 40., 20., 60., 20.]                        # primary array
y = [0.,  2.,  2.,  1.,  2.,  1.,  1.]                     # secondaryy array
ind = [0]*(n+1)                                            # array of indexes

print("Original arrays:")
print("x",end="")
for i in range(1,n+1): print("{0:6.2f}".format(x[i]),end="")
print()
print("y",end="")
for i in range(1,n+1): print("{0:6.2f}".format(y[i]),end="")
print()

Index2(x,y,ind,n)
print("Indexes:")
for i in range(1,n+1): print("{0:6d}".format(ind[i]),end="")
print()

print("Indexed arrays:")
print("x",end="")
for i in range(1,n+1): print("{0:6.2f}".format(x[ind[i]]),end="")
print()
print("y",end="")
for i in range(1,n+1): print("{0:6.2f}".format(y[ind[i]]),end="")
print()
