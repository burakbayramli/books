//-------------------------------- utils.h ----------------------------------
// Contains utility routines.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _UTILS_
#define _UTILS_

#include <math.h>

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif
#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

// Rounds x to the next integer (result of type double)
#define Round(x) floor(x + 0.5)

// Rounds x to the next integer (result of type int)
#define Nint(x) (int)floor(x + 0.5)

// Transfers the sign of y to the absolute value of x (result of type double)
#define Sign(x,y) (y >= 0.0 ? fabs(x) : -fabs(x))

//===========================================================================
double Magn(double x)
//---------------------------------------------------------------------------
// Returns the order of magnitude of x as 10^n
//---------------------------------------------------------------------------
{
   if (x)
      return (fabs(x) >= 1e0 ? pow(10e0,(int)(log10(fabs(x))))
                             : pow(0.1e0,(int)(fabs(log10(fabs(x))))+1e0));
   else return 0e0;
}

//===========================================================================
double Fact(int n)
//---------------------------------------------------------------------------
// Returns the factorial of n
//---------------------------------------------------------------------------
{
	double f;
	int i;

   f = 1e0;
   for (i=2; i<=n; i++) f *= i;
   return f;
}

//===========================================================================
long IFact(int n)
//---------------------------------------------------------------------------
// Returns the factorial of the integer n (result of type long int)
//---------------------------------------------------------------------------
{
	long i, f;

   f = 1;
   for (i=2; i<=n; i++) f *= i;
   return f;
}

#endif
