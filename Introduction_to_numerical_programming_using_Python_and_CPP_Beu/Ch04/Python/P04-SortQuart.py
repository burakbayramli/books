# Sorting the elements of an array into quartiles
from sort import *
from random import *

# main

n = 12                                        # number of values to be sorted
x = [0]*(n+1)                                            # array to be sorted
ind = [0]*(n+1); rnk = [0]*(n+1)                # arrays of indexes and ranks

print("Original array:")
for i in range(1,n+1):
   x[i] = random()                                # random sub-unitary values
   print("{0:6.2f}".format(x[i]),end="")
print("\n")

Index(x,ind,n)                                      # create array of indexes
Rank(ind,rnk,n)                                       # create array of ranks

print("Quartile 1")
print("     i   ind   rnk        x")
for i in range(1,int(n/4)+1):
   indi = ind[i]
   print("{0:6d}{1:6d}{2:6d}{3:10.2f}".format(i,ind[i],rnk[indi],x[indi]))

print("Quartile 2")
print("     i   ind   rnk        x")
for i in range(int(n/4)+1,int(n/2)+1):
   indi = ind[i]
   print("{0:6d}{1:6d}{2:6d}{3:10.2f}".format(i,ind[i],rnk[indi],x[indi]))

print("Quartile 3")
print("     i   ind   rnk        x")
for i in range(int(n/2)+1,int(3*n/4)+1):
   indi = ind[i]
   print("{0:6d}{1:6d}{2:6d}{3:10.2f}".format(i,ind[i],rnk[indi],x[indi]))

print("Quartile 4")
print("     i   ind   rnk        x")
for i in range(int(3*n/4)+1,n+1):
   indi = ind[i]
   print("{0:6d}{1:6d}{2:6d}{3:10.2f}".format(i,ind[i],rnk[indi],x[indi]))
