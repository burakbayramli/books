// Evaluates the exponential function
#include <stdio.h>
#include <math.h>
#include "elemfunc.h"

int main()
{
   double expx, Expx, Exp1x, x;
   
   for (x=-5e0; x<=5e0; x+=1e0) {
      expx = exp(x);                                              // built-in
      Expx = Exp(x);                                          // power series
      Exp1x = Exp1(x);                                  // continued fraction
      printf("%10f %15.7e %15.7e %15.7e %15.7e\n",
             x,expx,Expx,Exp1x,1e0-Exp1x/expx);
   }
}
