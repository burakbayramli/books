//------------------------------- specfunc.h --------------------------------
// Contains routines for evaluating special functions.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _SPECFUNC_
#define _SPECFUNC_

#include <stdlib.h>
#include <math.h>

//===========================================================================
double Chebyshev(int n, double x, double d)
//---------------------------------------------------------------------------
// Evaluates the n-th order Chebyshev polynomial and its derivative d in x
// using the recurrence relation
//---------------------------------------------------------------------------
{
   double f, fm1, fm2, x2;
   int i;

   if (n == 0) {
      f = 1e0; d = 0e0;
   } else {
      f = x; fm1 = 1e0; x2 = 2*x;
      for (i=2; i<=n; i++) {
         fm2 = fm1; fm1 = f;
         f = x2*fm1 - fm2;
      }
      d = (x*x-1e0) ? n*(x*f-fm1)/(x*x-1e0) : n*n*f/x;
   }
   return f;
}

//===========================================================================
double Legendre(int n, double x, double &d)
//---------------------------------------------------------------------------
// Evaluates the n-th order Legendre polynomial and its derivative d in x
// using the recurrence relation
//---------------------------------------------------------------------------
{
   double f, fm1, fm2;
   int i;

   if (n == 0) {
      f = 1e0; d = 0e0;
   } else {
      f = x; fm1 = 1e0;
      for (i=2; i<=n; i++) {
         fm2 = fm1; fm1 = f;
         f = ((2*i-1)*x*fm1 - (i-1)*fm2)/i;
      }
      d = (x*x-1e0) ? n*(x*f-fm1)/(x*x-1e0) : 0.5*n*(n+1)*f/x;
   }
   return f;
}

//===========================================================================
double aLegendre(int l, int m, double x)
//---------------------------------------------------------------------------
// Evaluates the associated Legendre function of orders l and m >= 0 in x
//---------------------------------------------------------------------------
{
   double p, pm1, pm2, sqx;
   int i;

   if (l < m) return 0e0;

   p = 1e0; pm1 = 0e0;                   // seed values: P(m,m,x), P(m-1,m,x)
   if (m) {
      sqx = -sqrt(1e0-x*x);
      for (i=1; i<=m; i++) p *= (2*i-1) * sqx;
   }

   for (i=m+1; i<=l; i++) {                                     // recurrence
      pm2 = pm1; pm1 = p;
      p = ((2*i-1)*x*pm1 - (i+m-1)*pm2)/(i-m);
   }
   return p;
}

//===========================================================================
double Laguerre(int n, double x, double &d)
//---------------------------------------------------------------------------
// Evaluates the n-th order Laguerre polynomial and its derivative d in x
// using the recurrence relation
//---------------------------------------------------------------------------
{
   double f, fm1, fm2;
   int i;

   if (n == 0) {
      f = 1e0; d = 0e0;
   } else {
      f = 1e0 - x; fm1 = 1e0;
      for (i=2; i<=n; i++) {
         fm2 = fm1; fm1 = f;
         f = ((2*i-1-x)*fm1 - (i-1)*fm2)/i;
      }
      d = x ? n*(f-fm1)/x : -n*f;
   }
   return f;
}

//===========================================================================
double aLaguerre(int n, int k, double x)
//---------------------------------------------------------------------------
// Evaluates the associated Laguerre polynomial of orders n and k in x
// using the recurrence relation
//---------------------------------------------------------------------------
{
   double f, fm1, fm2;
   int i;

   if (n == 0) {
      f = 1e0;
   } else {
      f = 1e0 + k - x; fm1 = 1e0;
      for (i=2; i<=n; i++) {
         fm2 = fm1; fm1 = f;
         f = ((2*i+k-1-x)*fm1 - (i+k-1)*fm2)/i;
      }
   }
   return f;
}

//===========================================================================
double Hermite(int n, double x, double &d)
//---------------------------------------------------------------------------
// Evaluates the n-th order Hermite polynomial and its derivative d in x
// using the recurrence relation
//---------------------------------------------------------------------------
{
   double f, fm1, fm2, x2;
   int i;

   if (n == 0) {
      f = 1e0; d = 0e0;
   } else {
      f = 2*x; fm1 = 1e0; x2 = 2*x;
      for (i=2; i<=n; i++) {
         fm2 = fm1; fm1 = f;
         f = x2*fm1 - 2*(i-1)*fm2;
      }
      d = 2*n*fm1;
   }
   return f;
}

//===========================================================================
void SpherY(int l, int m, double theta, double phi, double &ReY, double &ImY)
//---------------------------------------------------------------------------
// Evaluates the real and imaginary parts (ReY and ImY) of the spherical
// harmonic of orders l and m for arguments theta and phi.
// Calls aLegendre to calculate associated Legendre polynomials.
//---------------------------------------------------------------------------
{
#define pi 3.141592653589793
   double fact;
   int i, mabs;

   mabs = abs(m);

   fact = 1e0;
   for (i=l-mabs+1; i<=l+mabs; i++) fact *= i;           // (l+|m|)!/(l-|m|)!

   fact = sqrt((2*l+1)/(4e0*pi*fact)) * aLegendre(l,mabs,cos(theta));
   if (m < 0 && m % 2) fact = -fact;

   ReY = fact * cos(m*phi);
   ImY = fact * sin(m*phi);
}

//===========================================================================
double SBessj(int n, double x)
//---------------------------------------------------------------------------
// Evaluates iteratively the spherical Bessel function of order n in x
//---------------------------------------------------------------------------
{
   double j, j0, j1, j2, jn;
   int i, nmax;

   if (x == 0e0) return (n == 0 ? 1e0 : 0e0);
   j0 = sin(x)/x; if (n == 0) return j0;
   j1 = (j0 - cos(x))/x; if (n == 1) return j1;

   nmax = 0;                          // finds direction of stable recurrence
   if ((double)n >= fabs(x)) {        // nmax = 0 forward, nmax /= 0 backward
      jn = 1.;
      for (i=n; i<=(n+50); i++) {
         jn *= (2*i-1)/x;                           // net factor of increase
         if (jn >= 1e8) {nmax = i + 10; break;}     // for forward iteration
      }
   }

   if (nmax == 0) {                                      // forward iteration
      for (i=2; i<=n; i++) {
         j = (2*i-1)/x*j1 - j0;
         j0 = j1; j1 = j;
      }
      return j;
   } else {                                             // backward iteration
      j2 = 0.; j1 = 1e-20;
      for (i=nmax; i>=0; i--) {
         j = (2*i+3)/x*j1 - j2;
         j2 = j1; j1 = j;
         if (i == n) jn = j;                             // non-normalized jn
      }                                             
      return (j0/j)*jn;                                      // normalized jn
   }
}

//===========================================================================
double SBessy(int n, double x)
//---------------------------------------------------------------------------
// Evaluates iteratively the spherical Neumann function of order n in x
//---------------------------------------------------------------------------
{
   double y, y0, y1;
   int i;

   y0 = -cos(x)/x; if (n == 0) return y0;
   y1 = (y0 - sin(x))/x; if (n == 1) return y1;

   for (i=2; i<=n; i++) {
      y = (2*i-1)/x*y1 - y0;
      y0 = y1; y1 = y;
   }
   return y;
}

#endif
