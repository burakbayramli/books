// Ascending sort and indexing of an array
#include <stdio.h>
#include "sort.h"

int main()
{
   const int n = 6;                          // number of values to be sorted
   double x[] = {0., 30., 60., 50., 20., 10., 40.};     // array to be sorted
   int ind[n+1], rnk[n+1];                     // arrays of indexes and ranks
   int i;

   printf("Original array:\n");
   for (i=1; i<=n; i++) printf("%6.2f",x[i]); printf("\n");

   Index(x,ind,n);
   printf("Indexes:\n");
   for (i=1; i<=n; i++) printf("%6i",ind[i]); printf("\n");

   Rank(ind,rnk,n);
   printf("Ranks:\n");
   for (i=1; i<=n; i++) printf("%6i",rnk[i]); printf("\n");

   printf("Indexed array:\n");
   for (i=1; i<=n; i++) printf("%6.2f",x[ind[i]]); printf("\n");

   InsertSort(x,n);
   printf("Sorted array:\n");
   for (i=1; i<=n; i++) printf("%6.2f",x[i]); printf("\n");
}
