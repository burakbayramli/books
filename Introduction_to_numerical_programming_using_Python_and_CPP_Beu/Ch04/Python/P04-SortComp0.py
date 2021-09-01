# Comparison of operation counts for sorting methods
from random import *

#============================================================================
def BubbleSort(x, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[1..n] by modified bubble sort
#----------------------------------------------------------------------------
   global ncomp, nsave                            # no. of compares and saves

   ipass = 0                                        # initialize pass counter
   swap = 1                              # initialize swap flag to enter loop
   while (swap):                           # perform passes while swaps occur
      ipass += 1                                      # increase pass counter
      swap = 0                                         # initialize swap flag
      for i in range(1,n-ipass+1):              # loop over unsorted sublists
         ncomp += 1   #------------------------------------------------------
         if (x[i] > x[i+1]):                              # compare neighbors
            nsave += 3   #---------------------------------------------------
            xi = x[i]; x[i] = x[i+1]; x[i+1] = xi            # swap neighbors
            swap = 1                                          # set swap flag

#============================================================================
def InsertSort(x, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[1..n] by direct insertion
#----------------------------------------------------------------------------
   global ncomp, nsave                            # no. of compares and saves

   for ipiv in range(2,n+1):                               # loop over pivots
      nsave += 1   #---------------------------------------------------------
      xpiv = x[ipiv]                        # save pivot to free its location
      i = ipiv - 1                               # initialize sublist counter
      ncomp += 1   #---------------------------------------------------------
      while ((i > 0) and (x[i] > xpiv)):          # scan to the left of pivot
         nsave += 1   #------------------------------------------------------
         x[i+1] = x[i]                     # item > pivot: shift to the right
         i -= 1

      nsave += 1   #---------------------------------------------------------
      x[i+1] = xpiv                   # insert pivot into last freed location

#============================================================================
def QuickSort(x, l, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[l..n] by Quicksort
#----------------------------------------------------------------------------
   global ncomp, nsave                            # no. of compares and saves

   if (l >= n): return
                                    # pivot = x[n]; create "<" and ">=" lists
   m = l                                            # upper index in "<"-list
   for i in range(l,n):                   # scan entire list, excepting pivot
      ncomp += 1   #---------------------------------------------------------
      if (x[i] < x[n]):                    # compare current value with pivot
         nsave += 3   #------------------------------------------------------
         t = x[i]; x[i] = x[m]; x[m] = t    # swap < value to end of "<"-list
         m += 1                       # extend "<"-list: increase upper index

   nsave += 3   #------------------------------------------------------------
   t = x[m]; x[m] = x[n]; x[n] = t    # swap pivot between "<" and ">=" lists

   QuickSort(x,l,m-1)                                         # sort "<"-list

   QuickSort(x,m+1,n)                                        # sort ">="-list

# main

nf = 100                                                     # scaling factor
np = 50                                           # number of plotting points
ns = nf * np                             # max. number of values to be sorted
x = [0]*(ns+1); x0 = [0]*(ns+1)                 # array to be sorted and copy

out = open("sort.txt","w")                                 # open output file
out.write("       n            Bubble              "
            "Insertion           Quick\n")
out.write("               ncomp     nsave     ncomp"
          "     nsave     ncomp     nsave\n")
for ip in range(1,np+1):
   n = nf * ip                                # number of values to be sorted
   print("n = ",n)

   for i in range(1,n+1): x0[i] = random()                # list to be sorted

   for i in range(1,n+1): x[i] = x0[i]
   ncomp = 0; nsave = 0
   BubbleSort(x,n)
   out.write("{0:10d}{1:10d}{2:10d}".format(n,ncomp,nsave))

   for i in range(1,n+1): x[i] = x0[i]
   ncomp = 0; nsave = 0
   InsertSort(x,n)
   out.write("{0:10d}{1:10d}".format(ncomp,nsave))

   for i in range(1,n+1): x[i] = x0[i]
   ncomp = 0; nsave = 0
   QuickSort(x,1,n)
   out.write("{0:10d}{1:10d}\n".format(ncomp,nsave))

out.close()
