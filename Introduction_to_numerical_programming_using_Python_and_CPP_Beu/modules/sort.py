#---------------------------------- sort.py ---------------------------------
#  Contains routines for sorting, indexing and ranking numeric sequences.
#  Part of the numxlib numerics library. Author: Titus Beu, 2013
#----------------------------------------------------------------------------

#============================================================================
def BubbleSort0(x, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[1..n] by bubble sort
#----------------------------------------------------------------------------
   for ipass in range(1,n):                                  # loop of passes
      for i in range(1,n-ipass+1):              # loop over unsorted sublists
         if (x[i] > x[i+1]):                              # compare neighbors
            xi = x[i]; x[i] = x[i+1]; x[i+1] = xi            # swap neighbors

#============================================================================
def BubbleSort1(x, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[1..n] by modified bubble sort
#----------------------------------------------------------------------------
   for ipass in range(1,n):                                  # loop of passes
      swap = 0                                         # initialize swap flag
      for i in range(1,n-ipass+1):              # loop over unsorted sublists
         if (x[i] > x[i+1]):                              # compare neighbors
            xi = x[i]; x[i] = x[i+1]; x[i+1] = xi            # swap neighbors
            swap = 1                                          # set swap flag

      if (not swap): break                      # exit loop if no swap occurs

#============================================================================
def BubbleSort(x, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[1..n] by modified bubble sort
#----------------------------------------------------------------------------
   ipass = 0                                        # initialize pass counter
   swap = 1                              # initialize swap flag to enter loop
   while (swap):                           # perform passes while swaps occur
      ipass += 1                                      # increase pass counter
      swap = 0                                         # initialize swap flag
      for i in range(1,n-ipass+1):              # loop over unsorted sublists
         if (x[i] > x[i+1]):                              # compare neighbors
            xi = x[i]; x[i] = x[i+1]; x[i+1] = xi            # swap neighbors
            swap = 1                                          # set swap flag

#============================================================================
def InsertSort(x, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[1..n] by direct insertion
#----------------------------------------------------------------------------
   for ipiv in range(2,n+1):                               # loop over pivots
      xpiv = x[ipiv]                        # save pivot to free its location
      i = ipiv - 1                               # initialize sublist counter
      while ((i > 0) and (x[i] > xpiv)):          # scan to the left of pivot
         x[i+1] = x[i]                     # item > pivot: shift to the right
         i -= 1

      x[i+1] = xpiv                   # insert pivot into last freed location

#============================================================================
def QuickSort(x, l, n):
#----------------------------------------------------------------------------
#  Ascending sort of array x[l..n] by Quicksort
#----------------------------------------------------------------------------
   if (l >= n): return
                                    # pivot = x[n]; create "<" and ">=" lists
   m = l                                            # upper index in "<"-list
   for i in range(l,n):                   # scan entire list, excepting pivot
      if (x[i] < x[n]):                    # compare current value with pivot
         t = x[i]; x[i] = x[m]; x[m] = t    # swap < value to end of "<"-list
         m += 1                       # extend "<"-list: increase upper index

   t = x[m]; x[m] = x[n]; x[n] = t    # swap pivot between "<" and ">=" lists

   QuickSort(x,l,m-1)                                         # sort "<"-list

   QuickSort(x,m+1,n)                                        # sort ">="-list

#============================================================================
def Index(x, ind, n):
#----------------------------------------------------------------------------
#  Ascending indexing of array x[1..n] in ind[] by insertion sort
#----------------------------------------------------------------------------
   for i in range(1,n+1): ind[i] = i                 # initialize index array

   for ipiv in range(2,n+1):                               # loop over pivots
      xpiv = x[ipiv]                        # save pivot to free its location
      i = ipiv - 1                               # initialize sublist counter
      while ((i > 0) and (x[ind[i]] > xpiv)):     # scan to the left of pivot
         ind[i+1] = ind[i]                 # item > pivot: shift to the right
         i -= 1

      ind[i+1] = ipiv                 # insert pivot into last freed location

#============================================================================
def Rank(ind, rnk, n):
#----------------------------------------------------------------------------
#  Returns the ranks rnk[1..n] for a list indexed in ind[1..n]
#----------------------------------------------------------------------------
   for i in range(1,n+1): rnk[ind[i]] = i

#============================================================================
def Index2(x, y, ind, n):
#----------------------------------------------------------------------------
#  Ascending indexing of correlated arrays x[1..n] and y[1..n] in ind[] by
#  insertion sort using x[] as primary key and y[] as secondary key
#----------------------------------------------------------------------------
   for i in range(1,n+1): ind[i] = i                 # initialize index array

   for ipiv in range(2,n+1):                               # loop over pivots
      xpiv = x[ipiv]; ypiv = y[ipiv]
      i = ipiv - 1                               # initialize sublist counter
      while ((i > 0) and                          # scan to the left of pivot
             (x[ind[i]] > xpiv or (x[ind[i]] == xpiv and y[ind[i]] > ypiv))):
         ind[i+1] = ind[i]                 # item > pivot: shift to the right
         i -= 1

      ind[i+1] = ipiv                 # insert pivot into last freed location
