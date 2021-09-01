// Newton-Raphson method for systems of non-linear equations
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "roots.h"

void Func(double f[], double x[], int n)
{                                                    // zeros: (1, 2), (2, 1)
   f[1] = pow(x[1],3) * x[2] + x[1] * pow(x[2],3) - 10e0;
   f[2] = pow(x[1],2) * x[2] + x[1] * pow(x[2],2) - 6e0;
}

int main()
{
   double *f, *x, *dx;
   int i, n;

   n = 2;
   f = Vector(1,n);
   x = Vector(1,n);
   dx = Vector(1,n);

   x[1] = 0e0; x[2] = 0e0;                           // initial approximation

   NewtonSys(Func,x,dx,n);
   Func(f,x,n);

   printf("Solution:\n");
   printf("           x           dx        f\n");
   for (i=1; i<=n; i++)
      printf("%i  %15.7e  %7.0e  %7.0e\n",i,x[i],dx[i],f[i]);
}
