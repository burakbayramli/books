// Returning swapped arguments from a function (C syntax)
#include <stdio.h>

void Swap(double *x, double *y)              // formal arguments are pointers
{
   double temp;                                         // temporary variable

   temp = *x; *x = *y; *y = temp;  // variable values accessed by indirection
}

int main()
{
   double a, b;

   a = 1e0; b = 2e0;                                  // values to be swapped
   Swap(&a,&b);                             // actual arguments are addresses
   printf("%f %f\n",a,b);
}
