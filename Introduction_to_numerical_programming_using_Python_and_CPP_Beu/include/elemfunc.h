//------------------------------- elemfunc.h --------------------------------
// Contains routines for evaluating elementary functions.
// Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _ELEMFUNC_
#define _ELEMFUNC_

#include <math.h>

//===========================================================================
double Poly(double x, double a[], int n)
//---------------------------------------------------------------------------
// Evaluates the polynomial P(x) = a[0] x^n + a[1] x^(n-1) + ... + a[n] with
// real coefficients in x using Horner's scheme
//---------------------------------------------------------------------------
{
   double p;
   int i;

   p = a[0];
   for (i=1; i<=n; i++) p = p*x + a[i];
   return p;
}

//===========================================================================
void PolyDerive(double a[], double b[], int n)
//---------------------------------------------------------------------------
// For the real polynomial P(x) = a[0] x^n + ... + a[n], the function returns
// the coefficients of the derivative P'(x) = b[0] x^(n-1) + ... + b[n-1]
//---------------------------------------------------------------------------
{
   int i;

   for (i=0; i<=n; i++) b[i] = (n-i) * a[i];
}

//===========================================================================
void PolyDivide(double x0, double a[], double b[], int n)
//---------------------------------------------------------------------------
// For the real polynomial P(x) = a[0] x^n + ... + a[n], the function returns
// the coefficients of the division by the binomial (x-x0):
// P(x) = (x-x0) (b[0] x^(n-1) + ... + b[n-1]) + b[n] (b[n] = P(x0))
//---------------------------------------------------------------------------
{
   int i;

   b[0] = a[0];
   for (i=1; i<=n; i++) b[i] = b[i-1]*x0 + a[i];
}

//===========================================================================
double Exp0(double x)
//---------------------------------------------------------------------------
// Evaluates exp(x) from its power-series expansion
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                            // relative precision
   double f, t;
   int i;
   
   i = 0;
   f = t = 1e0;
   while (fabs(t) > eps*fabs(f)) {
      i++;
      t *= x/i;
      f += t;
   }
   return f;
}

//===========================================================================
double Exp(double x)
//---------------------------------------------------------------------------
// Evaluates exp(x) from its power-series expansion
// For x < 0 avoids potential instabilities due to subtractions
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                            // relative precision
   double absx, f, t;
   int i;
   
   i = 0;
   f = t = 1e0;
   absx = fabs(x);
   while (fabs(t) > eps*fabs(f)) {
      i++;
      t *= absx/i;
      f += t;
   }
   return (x >= 0.0 ? f : 1e0/f);
}

//===========================================================================
double Sin(double x)
//---------------------------------------------------------------------------
// Evaluates sin(x) from its power-series expansion
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                            // relative precision
   double f, t, x2;
   int i;
   
   i = 1;
   f = t = x;
   x2 = x*x;
   while (fabs(t) > eps*fabs(f)) {
      i += 2;
      t *= -x2/((i-1)*i);
      f += t;
   }
   return f;
}

//===========================================================================
double ArcSin(double x)
//---------------------------------------------------------------------------
// Evaluates arcsin(x) from its power-series expansion (|x| < 1)
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                            // relative precision
   double f, t, x2;
   int i, i2;
   
   i = 1;
   f = t = x;
   x2 = x*x;
   while (fabs(t) > eps*fabs(f)) {
      i2 = i*i;
      i += 2;
      t *= i2*x2/((i-1)*i);
      f += t;
   }
   return f;
}

//===========================================================================
double Tan(double x)
//---------------------------------------------------------------------------
// Evaluates tan(x) from its continued fraction representation
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                            // relative precision
   double a, b, p, pm1, pm2, q, qm1, qm2, r, rm1;

   a = 1e0; b = -x*x;
   pm1 = 0e0; p = x;
   qm1 = 1e0; q = 1e0;
   rm1 = 0e0; r = p/q;
   while (fabs(r-rm1) > eps*fabs(r)) {
      pm2 = pm1; pm1 = p;                                  // shift the stack
      qm2 = qm1; qm1 = q;
      rm1 = r;
      a += 2e0;
      p = a*pm1 + b*pm2;
      q = a*qm1 + b*qm2;
      r = p/q;                                        // new convergent value
   }
   return r;
}

//===========================================================================
double Exp1(double x)
//---------------------------------------------------------------------------
// Evaluates exp(x) from its continued fraction representation
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                            // relative precision
   double a, b, p, pm1, pm2, q, qm1, qm2, r, rm1;
   int i;

   a = 1e0; b = x;
   pm1 = 1e0; p = 1e0 + x;
   qm1 = 1e0; q = 1e0;
   rm1 = 1e0; r = p/q;
   i = 1;
   while (fabs(r-rm1) > eps*fabs(r)) {
      i++;
      pm2 = pm1; pm1 = p;                                  // shift the stack
      qm2 = qm1; qm1 = q;
      rm1 = r;
      a = (i%2 ? i : 2e0);
      b = -b;
      p = a*pm1 + b*pm2;
      q = a*qm1 + b*qm2;
      r = (q ? p/q : 9e99);                           // new convergent value
   }
   return r;
}

#endif
