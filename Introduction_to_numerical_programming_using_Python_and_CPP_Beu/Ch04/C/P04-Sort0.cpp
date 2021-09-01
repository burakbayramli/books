// Ascending sort of an array
#include <stdio.h>
#include "sort.h"

int main()
{
   const int n = 6;                          // number of values to be sorted
   double x[] = {0., 30., 60., 50., 20., 10., 40.};     // array to be sorted
   int i;

   printf("Original array:\n");
   for (i=1; i<=n; i++) printf("%6.2f",x[i]); printf("\n");

   BubbleSort(x,n);

   printf("Sorted array:\n");
   for (i=1; i<=n; i++) printf("%6.2f",x[i]); printf("\n");
}
