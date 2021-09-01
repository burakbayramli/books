//--------------------------------- roots.h ---------------------------------
// Contains routines for determining real roots of real functions.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _ROOTS_
#define _ROOTS_

#include <stdio.h>
#include <math.h>
#include "linsys.h"

//===========================================================================
int Bisect(double Func(double), double a, double b, double &x)
//---------------------------------------------------------------------------
// Determines a real root x of function Func isolated in interval [a,b] by
// the bisection method
// Error code: 0 - normal execution
//             1 - [a,b] does not isolate one root or contains several roots
//             2 - max. number of iterations exceeded
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                    // relative precision of root
   const int itmax = 100;                           // max. no. of iterations
   double fa, fb, fx;
   int it;

   x = a; fa = Func(x);                                     // is a the root?
   if (fabs(fa) == 0e0) return 0;
   x = b; fb = Func(x);                                     // is b the root?
   if (fabs(fb) == 0e0) return 0;

   if (fa*fb > 0) return 1;                  // [a,b] does not contain a root
                                             // or contains several roots
   for (it=1; it<=itmax; it++) {
      x = 0.5e0 * (a + b);                               // new approximation
      fx = Func(x);
      if (fa*fx > 0) a = x; else b = x;       // choose new bounding interval
      if (((b-a) <= eps*fabs(x)) || (fabs(fx) <= eps)) return 0;
   }
   printf("Bisect: max. no. of iterations exceeded !\n"); return 2;
}

//===========================================================================
int FalsPos(double Func(double), double a, double b, double &x)
//---------------------------------------------------------------------------
// Determines a real root x of function Func isolated in interval [a,b] by
// the false position method
// Error code: 0 - normal execution
//             1 - interval does not contain a root
//             2 - max. number of iterations exceeded
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                    // relative precision of root
   const int itmax = 100;                           // max. no. of iterations
   double dx, fa, fb, fx;
   int it;

   x = a; fa = Func(x);                                     // is a the root?
   if (fabs(fa) == 0e0) return 0;
   x = b; fb = Func(x);                                     // is b the root?
   if (fabs(fb) == 0e0) return 0;

   if (fa*fb > 0) return 1;                  // [a,b] does not contain a root
                                             // or contains several roots
   for (it=1; it<=itmax; it++) {
      x = (a*fb - b*fa)/(fb - fa);                       // new approximation
      fx = Func(x);
      if (fa*fx > 0) {                        // choose new bounding interval
         dx = x - a; a = x; fa = fx;
      } else {
         dx = b - x; b = x; fb = fx;
      }
      if ((fabs(dx) <= eps*fabs(x)) || (fabs(fx) <= eps)) return 0;
   }
   printf("FalsPos: max. no. of iterations exceeded !\n"); return 2;
}

//===========================================================================
int Iter(double Func(double), double a, double b, double &x)
//---------------------------------------------------------------------------
// Determines a root x of function Func isolated in [a,b] by the method of
// successive approximations. x contains on entry an initial approximation.
// Error code: 0 - normal execution
//             1 - interval does not contain a root
//             2 - max. number of iterations exceeded
//             3 - diverging process
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                  // relative precision criterion
   const int itmax = 100;                           // max. no. of iterations
   double dx, f;
   int it;

   dx = -Func(x);                                    // initialize correction
   for (it=1; it<=itmax; it++) {
      f = Func(x);
      if (fabs(f) > fabs(dx)) break;       // compare new with old correction
      dx = -f;                                           // update correction
      x += dx;                                           // new approximation
      if ((x < a) || (x > b)) return 1;   // interval does not contain a root
      if (fabs(dx) <= eps*fabs(x)) return 0;             // check convergence
   }
   if (it > itmax)
      { printf("Iter: max. no. of iterations exceeded !\n"); return 2; }
   if (fabs(f) > fabs(dx))
      { printf("Iter: diverging process !\n"); return 3; }
}

//===========================================================================
int Newton(double Func(double, double &), double a, double b, double &x)
//---------------------------------------------------------------------------
// Determines a real root x of function Func isolated in interval [a,b] by
// the Newton-Raphson method using the analytical derivative. x contains on
// input an initial approximation.
// Error code: 0 - normal execution
//             1 - interval does not contain a root
//             2 - max. number of iterations exceeded
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                  // relative precision criterion
   const int itmax = 100;                           // max. no. of iterations
   double df, dx, f;
   int it;

   for (it=1; it<=itmax; it++) {
      f = Func(x,df);                              // function and derivative
      dx = (fabs(df) > eps) ? -f/df : -f;                  // root correction
      x += dx;                                           // new approximation
      if ((x < a) || (x > b)) return 1;      // [a,b] does not contain a root
      if (fabs(dx) <= eps*fabs(x)) return 0;             // check convergence
   }
   printf("Newton: max. no. of iterations exceeded !\n"); return 2;
}

//===========================================================================
int NewtonNumDrv(double Func(double), double a, double b, double &x)
//---------------------------------------------------------------------------
// Determines a real root x of function Func isolated in interval [a,b] by
// the Newton-Raphson method using the numerical derivative. x contains on
// input an initial approximation.
// Error code: 0 - normal execution
//             1 - interval does not contain a root
//             2 - max. number of iterations exceeded
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                  // relative precision criterion
   const int itmax = 100;                           // max. no. of iterations
   double df, dx, f;
   int it;

   for (it=1; it<=itmax; it++) {
      f = Func(x);
      dx = x ? eps*fabs(x) : eps;                          // derivation step
      df = (Func(x+dx)-f)/dx;                         // numerical derivative
      dx = (fabs(df) > eps) ? -f/df : -f;                  // root correction
      x += dx;                                           // new approximation
      if ((x < a) || (x > b)) return 1;      // [a,b] does not contain a root
      if (fabs(dx) <= eps*fabs(x)) return 0;             // check convergence
   }
   printf("NewtonNumDrv: max. no. of iterations exceeded !\n"); return 2;
}

//===========================================================================
int Secant(double Func(double), double a, double b, double &x)
//---------------------------------------------------------------------------
// Determines a real root x of function Func isolated in interval [a,b] by
// the secant method. x contains on entry an initial approximation.
// Error code: 0 - normal execution
//             1 - interval does not contain a root
//             2 - max. number of iterations exceeded
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                  // relative precision criterion
   const int itmax = 100;                           // max. no. of iterations
   double df, dx, f, f0, x0;
   int it;

   x0 = x; f0 = Func(x0);
   x = x0 - f0;
   for (it=1; it<=itmax; it++) {
      f = Func(x);
      df = (f-f0)/(x-x0);                           // approximate derivative
      x0 = x; f0 = f;                    // store abscissa and function value
      dx = (fabs(df) > eps) ? -f/df : -f;                  // root correction
      x += dx;                                           // new approximation
      if ((x < a) || (x > b)) return 1;      // [a,b] does not contain a root
      if (fabs(dx) <= eps*fabs(x)) return 0;             // check convergence
   }
   printf("Secant: max. no. of iterations exceeded !\n"); return 2;
}

//===========================================================================
void BirgeVieta(double a[], int n, double xx[], int &nx)
//---------------------------------------------------------------------------
// Determines the real roots of a real polynomial by the Birge-Vieta method
// a[]   - coefficients of the polynomial
// n     - order of the polynomial
// xx[]  - array of roots (output)
// nx    - number of found roots (output)
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;                  // relative precision criterion
   const int itmax = 100;                           // max. no. of iterations
   double d, p, x;
   int it, j, m;

   nx = 0;
   if (n <= 1) return;

   x = 0;                               // initial approximation for 1st root
   for (m=n; m>=2; m--) {                    // loop over reduced polynomials
      for (it=1; it<=itmax; it++) {                  // Newton-Raphson method
         p = a[0]; d = p;                                  // Horner's scheme
         for (j=1; j<=m-1; j++) {
            p = p*x + a[j];                                     // polynomial
            d = d*x + p;                                        // derivative
         }
         p = p*x + a[m];
         d = (d) ? -p/d : -p;                              // root correction
         x += d;
         if (fabs(d) <= eps*fabs(x)) break;              // check convergence
      }
      if (it == itmax) {
         printf("Birge: max. no. of iterations exceeded !\n"); return;
      }
      nx++;
      xx[nx] = x;                                               // store root
                                    // coefficients of new reduced polynomial
      for (j=1; j<=(m-1); j++) a[j] += a[j-1]*x;
   }
   nx++;
   xx[nx] = -a[1]/a[0];                       // root of 1st order polynomial
}

//===========================================================================
void Jacobian(double x[], double **jac, int n,
              void Func(double[],double[],int))
//---------------------------------------------------------------------------
// Calculates the Jacobian of a system of n real functions with n variables
// using central finite differences
// x[]     - point at which the Jacobian is evaluated
// jac[][] - Jacobian
// n       - space dimension
// Func    - user function returning the function values at point x
//              void Func(double f[], double x[], int n);
//---------------------------------------------------------------------------
{
   const double eps = 1e-10;
   int i, j;
   double h, h2, *fm, *fp, x0;

   fm = Vector(1,n);
   fp = Vector(1,n);

   for (j=1; j<=n; j++) {                            // loop over coordinates
      x0 = x[j];                                                // store x[j]
      h = x0 ? eps*fabs(x0) : eps;                               // step-size
      x[j] = x0 - h; Func(fm,x,n);                          // decrement x[j]
      x[j] = x0 + h; Func(fp,x,n);                          // increment x[j]
      h2 = 1e0/(2e0*h);
      for (i=1; i<=n; i++) jac[i][j] = (fp[i] - fm[i]) * h2;      // Jacobian
      x[j] = x0;                                              // restore x[j]
   }

   FreeVector(fm,1);
   FreeVector(fp,1);
}

//===========================================================================
int NewtonSys(void Func(double [], double [], int),
              double x[], double dx[], int n)
//---------------------------------------------------------------------------
// Determines a n-dimensional real zero of a system of n real functions by
// Newton-Raphson method.
// Func - user function returning the function values f[] for arguments x[]
//           void Func(double f[], double x[], int n);
// x[]  - initial approximation (input), solution (output)
// dx[] - error estimates of the solution components (output)
// n    - order of system
// ierr - error code: 0 - normal execution
//                    1 - max. number of iterations exceeded
// Calls: Jacobian - computes Jacobian
//        MatInv   - inverts (n x n) matrix (in linsys.h)
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                 // precision for cumulated error
   const int itmax = 100;                           // max. no. of iterations
   double *f, **jac;
   double det, err, sum;
   int i, ierr, it, j;

   f   = Vector(1,n);
   jac = Matrix(1,n,1,n);

   for (it=1; it<=itmax; it++) {
      Func(f,x,n);                                               // functions
      Jacobian(x,jac,n,Func);                                     // Jacobian
      MatInv(jac,n,det);                                  // inverse Jacobian

      if (det) {                                               // corrections
         for (i=1; i<=n; i++) {                      // non-singular Jacobian
            sum = 0e0;
            for (j=1; j<=n; j++) sum -= jac[i][j] * f[j];
            dx[i] = sum;
         }
      } else { for (i=1; i<=n; i++) dx[i] = -f[i]; }     // singular Jacobian

      err = 0e0;
      for (i=1; i<=n; i++) {
         x[i] += dx[i];                                  // new approximation
         err += fabs(f[i]);                                 // cumulate error
      }
      if (err <= eps) break;                             // check convergence
   }

   FreeVector(f,1);
   FreeMatrix(jac,1,1);

   ierr = 0;
   if (it >= itmax)
      { ierr = 1; printf("NewtonSys: max. no. of iterations exceeded !\n"); }
   return ierr;
}

#endif
