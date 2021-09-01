// Ascending indexing of two correlated arrays
#include <stdio.h>
#include "sort.h"

int main()
{
   const int n = 6;                          // number of values to be sorted
   double x[] = {0., 40., 60., 40., 20., 60., 20.};          // primary array
   double y[] = {0.,  2.,  2.,  1.,  2.,  1.,  1.};        // secondary array
   int ind[n+1];                                          // array of indexes
   int i;

   printf("Original arrays:\n");
   printf("x");
   for (i=1; i<=n; i++) printf("%6.2f",x[i]); printf("\n");
   printf("y");
   for (i=1; i<=n; i++) printf("%6.2f",y[i]); printf("\n");

   Index2(x,y,ind,n);
   printf("Indexes:\n");
   for (i=1; i<=n; i++) printf("%6i",ind[i]); printf("\n");

   printf("Indexed arrays:\n");
   printf("x");
   for (i=1; i<=n; i++) printf("%6.2f",x[ind[i]]); printf("\n");
   printf("y");
   for (i=1; i<=n; i++) printf("%6.2f",y[ind[i]]); printf("\n");
}
