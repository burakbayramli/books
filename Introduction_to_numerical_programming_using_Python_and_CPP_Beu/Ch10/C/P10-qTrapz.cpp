// Evaluates an integral using the trapezoidal rule
#include <stdio.h>
#include <math.h>

double Func(double x) { return (x*x*x) * exp(-x); }

//===========================================================================
double qTrapz(double Func(double), double a, double b, int n)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,b] using the trapezoidal rule
// with n integration points
//---------------------------------------------------------------------------
{
   double h, s;
   int i;

   h = (b-a)/(n-1);
   s = 0.5*(Func(a) + Func(b));
   for (i=1; i<=(n-2); i++) s += Func(a+i*h);
   
   return h*s;
}

int main()
{
   double a, b;
   int n;
   
   a = 0e0; b = 1e0; n = 100;
   printf("I = %f\n",qTrapz(Func,a,b,n));
}
