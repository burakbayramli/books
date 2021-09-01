// Real root of a real function by the method of successive approximations
#include <stdio.h>
#include <math.h>
#include "roots.h"

double func(double x) { return x - exp(-x); }

int main()
{
   double a, b, x;

   a = -1e10; b = 1e10;                                    // search interval
   x = 0e0;                                          // initial approximation

   if (Iter(func,a,b,x) == 0) {
      printf("x = %8.5f   f(x) = %7.0e\n",x,func(x));
   } else { printf("No solution found !\n"); }
}
