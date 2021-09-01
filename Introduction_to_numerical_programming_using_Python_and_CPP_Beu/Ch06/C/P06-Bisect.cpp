// Real roots of a real function by the bisection method
#include <stdio.h>
#include <math.h>
#include "roots.h"

double func(double x)
{
   return (x*x*x*x - 5e0*x*x + 4e0) * sin(2e0*x);
}

int main()
{
   double a, b, h, x, xmin, xmax;
   int ierr;

   xmin = -3.5e0;                             // limits of root search domain
   xmax =  3.5e0;
   h = 0.1e0;                           // width of root separation intervals

   a = xmin;
   while (a < xmax) {                                 // root separation loop
      b = a + h;                                     // search interval [a,b]
      ierr = Bisect(func,a,b,x);
      if ((ierr == 0) && (x != b))
         printf("x = %8.5f in (%6.2f,%6.2f)  f(x) = %7.0e\n",x,a,b,func(x));
      a = b;                                           // shift left boundary
   }
}
