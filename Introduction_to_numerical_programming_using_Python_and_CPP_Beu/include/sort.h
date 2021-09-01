//--------------------------------- sort.h ----------------------------------
// Contains routines for sorting, indexing and ranking numeric sequences.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _SORT_
#define _SORT_

//===========================================================================
void BubbleSort0(double x[], int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[1..n] by bubble sort
//---------------------------------------------------------------------------
{
   double xi;
   int i, ipass;

   for (ipass=1; ipass<=(n-1); ipass++)                     // loop of passes
      for (i=1; i<=n-ipass; i++)               // loop over unsorted sublists
         if (x[i] > x[i+1])                              // compare neighbors
            { xi = x[i]; x[i] = x[i+1]; x[i+1] = xi; }      // swap neighbors
}

//===========================================================================
void BubbleSort1(double x[], int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[1..n] by modified bubble sort
//---------------------------------------------------------------------------
{
   double xi;
   int i, ipass, swap;

   for (ipass=1; ipass<=(n-1); ipass++) {                   // loop of passes
      swap = 0;                                       // initialize swap flag
      for (i=1; i<=n-ipass; i++)               // loop over unsorted sublists
         if (x[i] > x[i+1]) {                            // compare neighbors
            xi = x[i]; x[i] = x[i+1]; x[i+1] = xi;          // swap neighbors
            swap = 1;                                        // set swap flag
         }
      if (!swap) break;                        // exit loop if no swap occurs
   }
}

//===========================================================================
void BubbleSort(double x[], int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[1..n] by modified bubble sort
//---------------------------------------------------------------------------
{
   double xi;
   int i, ipass, swap;

   ipass = 0;                                      // initialize pass counter
   swap = 1;                            // initialize swap flag to enter loop
   while (swap) {                         // perform passes while swaps occur
      ipass ++;                                      // increase pass counter
      swap = 0;                                       // initialize swap flag
      for (i=1; i<=n-ipass; i++)               // loop over unsorted sublists
         if (x[i] > x[i+1]) {                            // compare neighbors
            xi = x[i]; x[i] = x[i+1]; x[i+1] = xi;          // swap neighbors
            swap = 1;                                        // set swap flag
         }
   }
}

//===========================================================================
void InsertSort(double x[], int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[1..n] by direct insertion
//---------------------------------------------------------------------------
{
   double xpiv;
   int i, ipiv;

   for (ipiv=2; ipiv<=n; ipiv++) {                        // loop over pivots
      xpiv = x[ipiv];                      // save pivot to free its location
      i = ipiv - 1;                             // initialize sublist counter
      while ((i > 0) && (x[i] > xpiv)) {         // scan to the left of pivot
         x[i+1] = x[i];                   // item > pivot: shift to the right
         i--;
      }
      x[i+1] = xpiv;                 // insert pivot into last freed location
   }
}

//===========================================================================
void QuickSort(double x[], int l, int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[l..n] by Quicksort
//---------------------------------------------------------------------------
{
   double t;
   int i, m;

   if (l >= n) return;
                                   // pivot = x[n]; create "<" and ">=" lists
   m = l;                                          // upper index in "<"-list
   for (i=l; i<=n-1; i++)                // scan entire list, excepting pivot
      if (x[i] < x[n]) {                  // compare current value with pivot
         t = x[i]; x[i] = x[m]; x[m] = t;  // swap < value to end of "<"-list
         m++;                        // extend "<"-list: increase upper index
      }
   t = x[m]; x[m] = x[n]; x[n] = t;  // swap pivot between "<" and ">=" lists

   QuickSort(x,l,m-1);                                       // sort "<"-list

   QuickSort(x,m+1,n);                                      // sort ">="-list
}

//===========================================================================
void Index(double x[], int ind[], int n)
//---------------------------------------------------------------------------
// Ascending indexing of array x[1..n] in ind[] by insertion sort
//---------------------------------------------------------------------------
{
   double xpiv;
   int i, ipiv;

   for (i=1; i<=n; i++) ind[i] = i;                 // initialize index array

   for (ipiv=2; ipiv<=n; ipiv++) {                        // loop over pivots
      xpiv = x[ipiv];                      // save pivot to free its location
      i = ipiv - 1;                             // initialize sublist counter
      while ((i > 0) && (x[ind[i]] > xpiv)) {    // scan to the left of pivot
         ind[i+1] = ind[i];               // item > pivot: shift to the right
         i--;
      }
      ind[i+1] = ipiv;               // insert pivot into last freed location
   }
}

//===========================================================================
void Rank(int ind[], int rnk[], int n)
//---------------------------------------------------------------------------
// Returns the ranks rnk[1..n] for a list indexed in ind[1..n]
//---------------------------------------------------------------------------
{
   int i;

   for (i=1; i<=n; i++) rnk[ind[i]] = i;
}

//===========================================================================
void Index2(double x[], double y[], int ind[], int n)
//---------------------------------------------------------------------------
// Ascending indexing of correlated arrays x[1..n] and y[1..n] in ind[] by
// insertion sort using x[] as primary key and y[] as secondary key
//---------------------------------------------------------------------------
{
   double xpiv, ypiv;
   int i, ipiv;

   for (i=1; i<=n; i++) ind[i] = i;                 // initialize index array

   for (ipiv=2; ipiv<=n; ipiv++) {                        // loop over pivots
      xpiv = x[ipiv]; ypiv = y[ipiv];
      i = ipiv - 1;                             // initialize sublist counter
      while ((i > 0) &&                          // scan to the left of pivot
             (x[ind[i]] > xpiv || (x[ind[i]] == xpiv && y[ind[i]] > ypiv))) {
         ind[i+1] = ind[i];               // item > pivot: shift to the right
         i--;
      }
      ind[i+1] = ipiv;               // insert pivot into last freed location
   }
}

#endif
