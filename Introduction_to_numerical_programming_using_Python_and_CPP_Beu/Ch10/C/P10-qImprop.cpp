// Evaluates improper integrals
#include <stdio.h>
#include <math.h>
#include "integral.h"

double Func1(double x) { return x * exp(-x*x); }
double Func2(double x) { return (x ? sin(x)/x : 1e0); }
double Func3(double x) { return 1e0/sqrt(x); }

int main()
{
   double eps, xinf;
   
   xinf = 1e10; eps = 1e-8;
   printf("I1 = %f xinf = %f\n",qImprop1(Func1,0e0,xinf,eps),xinf);
   printf("I2 = %f xinf = %f\n",qImprop1(Func2,0e0,xinf,eps),xinf);
   printf("I3 = %f\n",qImprop2(Func3,0e0,1e0,eps));
}
