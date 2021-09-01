// Compare solvers for algebraic and transcendental equations
#include <stdio.h>
#include <math.h>
int it;       // comment out "int it;" in Bisect and Secant to make it global
#include "roots.h"

double func(double x) { return x - sin(x) - 0.25e0; }

int main()
{
   double a, b, x;

   a = -30e0; b = 30e0;                                    // search interval
   if (Bisect(func,a,b,x) == 0)
      printf("Bisect: x = %11.9f   f(x) = %7.0e   it = %i\n",x,func(x),it);

   x = -30e0;                                        // initial approximation
   if (Secant(func,a,b,x) == 0)
      printf("Secant: x = %11.9f   f(x) = %7.0e   it = %i\n",x,func(x),it);
}
