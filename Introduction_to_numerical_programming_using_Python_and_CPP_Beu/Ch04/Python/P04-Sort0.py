# Ascending sort of an array
from sort import *

# main

n = 6                                         # number of values to be sorted
x = [0., 30., 60., 50., 20., 10., 40.]                   # array to be sorted

print("Original array:")
for i in range(1,n+1): print("{0:6.2f}".format(x[i]),end="")
print()

BubbleSort(x,n)

print("Sorted array:")
for i in range(1,n+1): print("{0:6.2f}".format(x[i]),end="")
print()
