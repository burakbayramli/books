// Returning swapped arguments from a function (C++ syntax)
#include <stdio.h>

void Swap(double &x, double &y)              // arguments passed by reference
{
   double temp;                                         // temporary variable

   temp = x; x = y; y = temp;
}

void main()
{
   double a, b;

   a = 1e0; b = 2e0;                                  // values to be swapped
   Swap(a,b);
   printf("%f %f\n",a,b);
}
