// Evaluates an integral using adaptive classical quadratures
#include <stdio.h>
#include <math.h>
#include "integral.h"

double Func(double x) { return (x*x*x) * exp(-x); }

int main()
{
   double a, b, eps;
   
   a = 0e0; b = 1e0; eps = 1e-14;

   printf("I TrapzCtrl   = %16.14f\n",qTrapzCtrl(Func,a,b,eps));
   printf("I SimpsonCtrl = %16.14f\n",qSimpsonCtrl(Func,a,b,eps));
   printf("I Romberg     = %16.14f\n",qRomberg(Func,a,b,eps));
}
