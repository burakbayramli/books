// Real roots of a real function by the Newton-Raphson method
#include <stdio.h>
#include <math.h>
#include "roots.h"

double func(double x, double &df)
{
   df = 1e0 + exp(-x);
   return x - exp(-x);
}

double func1(double x, double &df)
{
   df = 1e0 - cos(x);
   return x - sin(x) - 0.25e0;
}

double func2(double x, double &df)
{
   df = (4e0*x*x*x - 10e0*x) * sin(2e0*x)
      + (x*x*x*x - 5e0*x*x + 4e0) * 2e0*cos(2e0*x);
   return (x*x*x*x - 5e0*x*x + 4e0) * sin(2e0*x);
}

int main()
{
   double a, b, df, h, x, xmin, xmax;
   int ierr;

   xmin = -3.5e0;                             // limits of root search domain
   xmax =  3.5e0;
   h = 0.1e0;                           // width of root separation intervals

   a = xmin;
   while (a < xmax) {                                 // root separation loop
      b = a + h;                                     // search interval [a,b]
      x = 0.5*(a+b);                                 // initial approximation
      ierr = Newton(func,a,b,x);
      if ((ierr == 0) && (x != b))
         printf("x = %8.5f in (%6.2f,%6.2f)  f(x) = %7.0e\n",x,a,b,func(x,df));
      a = b;                                           // shift left boundary
   }
}
