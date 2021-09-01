// Newton-Raphson method for systems of non-linear equations
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "roots.h"

void Func(double f[], double x[], int n)
{                                                          // zero: (0, 0, 0)
   f[1] = x[3] - x[1]*x[2]*x[3];
   f[2] = x[1] + x[2] + x[3];
   f[3] = x[1]*x[2] + x[2]*x[3] + x[3]*x[1];
}

int  main()
{
   double *f, *x, *dx;
   int i, n;

   n = 3;
   f = Vector(1,n);
   x = Vector(1,n);
   dx = Vector(1,n);

   x[1] = 1e0; x[2] = 2e0; x[3] = 3e0;               // initial approximation

   NewtonSys(Func,x,dx,n);
   Func(f,x,n);

   printf("Solution:\n");
   printf("           x           dx        f\n");
   for (i=1; i<=n; i++)
      printf("%i  %15.7e  %7.0e  %7.0e\n",i,x[i],dx[i],f[i]);
}
