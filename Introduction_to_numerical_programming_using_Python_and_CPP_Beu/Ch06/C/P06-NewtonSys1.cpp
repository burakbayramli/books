// Intersection of circle and parabola
#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "roots.h"

void Func(double f[], double x[], int n)
{                                   // zeros: (-2.295, 2.267), (2.583, 3.673)
   f[1] = pow(x[1]-1e0,2) + x[2]*x[2] - 16e0;
   f[2] = x[1]*x[1] - x[2] - 3e0;
}

int main()
{
   double *f, *x, *dx;
   int i, n;

   n = 2;
   f = Vector(1,n);
   x = Vector(1,n);
   dx = Vector(1,n);

   x[1] = -5e0; x[2] = 5e0;   // 1st initial approximation -> (-2.295, 2.267)
   NewtonSys(Func,x,dx,n);
   Func(f,x,n);

   printf("Solution 1:\n");
   printf("           x           dx        f\n");
   for (i=1; i<=n; i++)
      printf("%i  %15.7e  %7.0e  %7.0e\n",i,x[i],dx[i],f[i]);

   x[1] = 5e0; x[2] = 5e0;     // 2nd initial approximation -> (2.583, 3.673)
   NewtonSys(Func,x,dx,n);
   Func(f,x,n);

   printf("\nSolution 2:\n");
   printf("           x           dx        f\n");
   for (i=1; i<=n; i++)
      printf("%i  %15.7e  %7.0e  %7.0e\n",i,x[i],dx[i],f[i]);
}
