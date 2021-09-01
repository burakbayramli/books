// Sorting the elements of an array into quartiles
#include <stdio.h>
#include <stdlib.h>
#include "sort.h"

int main()
{
   const int n = 12;                         // number of values to be sorted
   double x[n+1];                                       // array to be sorted
   int ind[n+1], rnk[n+1];                     // arrays of indexes and ranks
   int i, indi;

   printf("Original array:\n");
   for (i=1; i<=n; i++) {
      x[i] = rand()/(RAND_MAX+1e0);              // random sub-unitary values
      printf("%6.2f",x[i]);
   }
   printf("\n");

   Index(x,ind,n);                                 // create array of indexes
   Rank(ind,rnk,n);                                  // create array of ranks

   printf("Quartile 1\n");
   printf("     i   ind   rnk        x\n");
   for (i=1; i<=int(n/4); i++) {
      indi = ind[i];
      printf("%6i%6i%6i%10.2f\n",i,ind[i],rnk[indi],x[indi]);
   }

   printf("Quartile 2\n");
   printf("     i   ind   rnk        x\n");
   for (i=int(n/4)+1; i<=int(n/2); i++) {
      indi = ind[i];
      printf("%6i%6i%6i%10.2f\n",i,ind[i],rnk[indi],x[indi]);
   }

   printf("Quartile 3\n");
   printf("     i   ind   rnk        x\n");
   for (i=int(n/2)+1; i<=int(3*n/4); i++) {
      indi = ind[i];
      printf("%6i%6i%6i%10.2f\n",i,ind[i],rnk[indi],x[indi]);
   }

   printf("Quartile 4\n");
   printf("     i   ind   rnk        x\n");
   for (i=int(3*n/4)+1; i<=n; i++) {
      indi = ind[i];
      printf("%6i%6i%6i%10.2f\n",i,ind[i],rnk[indi],x[indi]);
   }
}
