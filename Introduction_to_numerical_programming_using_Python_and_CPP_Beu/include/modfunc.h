//-------------------------------- modfunc.h --------------------------------
// Routines for modeling tabular dependences by interpolation/regression.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _MODFUNC_
#define _MODFUNC_

#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "linsys.h"

//===========================================================================
double Lagrange(double x[], double y[], int n, double xi)
//---------------------------------------------------------------------------
// Evaluates the Lagrange interpolating polynomial of n data points at xi
// x[] - x-coordinates of data points
// y[] - y-coordinates of data points
// n   - number of data points
// xi  - interpolation argument
//---------------------------------------------------------------------------
{
   double p, yi;
   int i, j;

   yi = 0e0;
   for (i=1; i<=n; i++) {
      p = 1e0;
      for (j=1; j<=n; j++)
         if (j != i) p *= (xi - x[j])/(x[i] - x[j]);
      yi += p * y[i];
   }
   return yi;
}

//===========================================================================
void Lagrange1(double x[], double y[], int n,
               double xi[], double yi[], int ni)
//---------------------------------------------------------------------------
// Evaluates the Lagrange interpolating polynomial of n data points on a mesh
// of ni interpolation points
// x[]  - x-coordinates of data points
// y[]  - y-coordinates of data points
// n    - number of data points
// xi[] - interpolation arguments
// yi[] - interpolant values (output)
// ni   - number of interpolation points
//---------------------------------------------------------------------------
{
   double p, xk, yk, *yf;
   int i, j, k;

   yf = Vector(1,n);                            // factors of the interpolant

   for (i=1; i<=n; i++) {     // prepare invariant factors of the interpolant
      p = 1e0;
      for (j=1; j<=n; j++)
         if (j != i) p *= (x[i] - x[j]);
      yf[i] = y[i] / p;
   }

   for (k=1; k<=ni; k++) {              // loop over the interpolation points
      xk = xi[k]; yk = 0e0;
      for (i=1; i<=n; i++) {
         p = 1e0;
         for (j=1; j<=n; j++)
            if (j != i) p *= (xk - x[j]);
         yk += p * yf[i];
      }
      yi[k] = yk;
   }
   FreeVector(yf,1);
}

//===========================================================================
double Neville(double x[], double y[], int n, double xi, double &err)
//---------------------------------------------------------------------------
// Evaluates the Lagrange interpolating polynomial of n data points at xi by
// Neville's method and returns an estimate of the absolute error
// x[] - x-coordinates of data points
// y[] - y-coordinates of data points
// n   - number of data points
// xi  - interpolation argument
// err - estimate of the absolute error (output)
//---------------------------------------------------------------------------
{
   double *p, yi;
   int i, m;

   p = Vector(1,n);                      // values of successive interpolants

   for (i=1; i<=n; i++) p[i] = y[i];    // initialize with 0-order polynomial

   for (m=1; m<=n-1; m++)                // loop over columns of scheme array
      for (i=1; i<=n-m; i++)                     // increase polynomial order
         p[i] = ((xi-x[m+i])*p[i] + (x[i]-xi)*p[i+1]) / (x[i]-x[m+i]);

   yi = p[1];                                             // polynomial value
   err = fabs(p[1] - p[2]);                                 // error estimate

   FreeVector(p,1);
   return yi;
}

//===========================================================================
void Spline(double x[], double y[], int n, double d1, double dn, int iopt,
            double a[], double b[], double c[], double d[],
            double xi[], double yi[], int ni)
//---------------------------------------------------------------------------
// Calculates the coefficients of the cubic splines for n data points and
// returns interpolated values on a mesh of ni points
// x[]    - x-coordinates of data points
// y[]    - y-coordinates of data points
// n      - number of data points
// d1, dn - 1st derivatives at the endpoints x[1] and x[n]
// iopt   - iopt == 0, natural splines: 2nd derivative = 0 at x[1] and x[n]
//          iopt == 1, clamped splines: uses the provided d1 and dn
// a[], b[], c[], d[], i=1..n
//        - coefficients of cubic splines i=1..n-1 (output)
// xi[]   - interpolation arguments
// yi[]   - interpolant values (output)
// ni     - number of interpolation points
// Calls: TriDiagSys (linsys.h)
//---------------------------------------------------------------------------
{
   double di, dm, hi, hm, xx, xp;
   int i, ip;

   if (iopt == 0) d1 = dn = 0e0;        // initialization for natural splines

   hi = 0e0; di = d1;           // coefficients for system of 2nd derivatives
   for (i=1; i<=(n-1); i++) {
      hm = hi; hi = x[i+1] - x[i];
      dm = di; di = (y[i+1] - y[i])/hi;
      a[i] = hm; b[i] = 2e0*(hm + hi); c[i] = hi; d[i] = 6e0*(di - dm);
   }
   a[n] = hi; b[n] = 2e0*hi; c[n] = 0e0; d[n] = 6e0*(dn - di);

   if (iopt == 0) c[1] = d[1] = a[n] = d[n] = 0e0;         // natural splines

   TriDiagSys(a,b,c,d,n);                         // solve tridiagonal system
                                                      // 2nd derivatives in d
   for (i=1; i<=(n-1); i++) {                          // spline coefficients
      ip = i + 1;
      xx = x[i]; xp = x[ip]; hi = xp - xx;
      a[i] = (d[ip] - d[i]) / (6e0*hi);
      b[i] = (d[i]*xp - d[ip]*xx) / (2e0*hi);
      c[i] = (d[ip]*xx*xx - d[i]*xp*xp) / (2e0*hi)
                                          + (y[ip] - y[i]) / hi - a[i]*hi*hi;
      d[i] = (d[i]*xp*xp*xp - d[ip]*xx*xx*xx) / (6e0*hi)
                                + (y[i]*xp - y[ip]*xx) / hi - b[i]*hi*hi/3e0;
   }

   for (i=1; i<=ni; i++) {                              // interpolation loop
      xx = xi[i];
      ip = 1; while (ip < n-1 && xx > x[ip+1]) ip++;       // index of spline
      yi[i] = ((a[ip]*xx + b[ip])*xx + c[ip])*xx + d[ip];  // evaluate spline
   }
}

//===========================================================================
void LinFit(double x[], double y[], double sigmy[], int n, int iopt,
            double &a, double &b, double &sigma, double &sigmb, double &chi2)
//---------------------------------------------------------------------------
// Determines the coefficients a and b of a linear model P(x;a,b) = a * x + b
// and the associated uncertainties, sigma and sigmb, for a set of observed
// data points by "Chi-square" regression
// x[]     - x-coordinates of observed data, i = 1,..,n
// y[]     - y-coordinates of observed data
// sigmy[] - standard deviations of observed data
// n       - number of observed data points
// iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
//         - iopt != 0 - uses the received values of sigmy[i]
// a, b    - parameters of the linear model (output)
// sigma   - uncertainties associated with the model parameters (output)
// sigmb
// chi2    - value of Chi-square merit function for the output parameters
//---------------------------------------------------------------------------
{
   double f, s, sx, sy, sxx, sxy;
   int i;

   if (iopt == 0)
      for (i=1; i<=n; i++) sigmy[i] = 1e0;        // iopt = 0: equall weights

   s = sx = sy = sxx = sxy = 0e0;                             // prepare sums
   for (i=1; i<=n; i++) {
      f = 1e0/(sigmy[i]*sigmy[i]);
      s  += f;
      sx += x[i] * f; sxx += x[i] * x[i] * f;
      sy += y[i] * f; sxy += x[i] * y[i] * f; 
   }

   f = 1e0/(s*sxx - sx*sx);
   a = (s *sxy - sx*sy ) * f; sigma = sqrt(s*f);         // model parameters
   b = (sy*sxx - sx*sxy) * f; sigmb = sqrt(sxx*f);       // and uncertainties

   chi2 = 0e0;                                // value of Chi-square function
   for (i=1; i<=n; i++) {
      f = (y[i] - a*x[i] - b)/sigmy[i];
      chi2 += f*f;
   }
}

//===========================================================================
void MultiFit(double x[], double y[], double sigmy[], int n, int iopt,
              double a[], double sigma[], int npar, double &chi2,
              void Func(double, double [], int))
//---------------------------------------------------------------------------
// Determines the coefficients a[i], i=1,..,npar of a multilinear model
//    F(x;a) = a[1] * func[1](x) + ... + a[npar] * func[npar](x)
// and the associated uncertainties, sigma[i], for a set of observed data
// points by "Chi-square" regression
// x[]     - x-coordinates of observed data, i = 1,..,n
// y[]     - y-coordinates of observed data
// sigmy[] - standard deviations of observed data
// n       - number of observed data points
// iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
//         - iopt != 0 - uses the received values of sigmy[i]
// a[]     - parameters of the multilinear model (output)
// sigma[] - uncertainties associated with the model parameters (output)
// npar    - number of model parameters
// chi2    - value of Chi-square merit function for the output parameters
// Func()  - user function returning for a given argument x the values of the
//           basis functions func[i](x):
//              void Func(double x, double func[], int npar);
// Calls: GaussJordan (linsys.h)
//---------------------------------------------------------------------------
{
   double det, f, fj, yi;
   double **b, **c, *func;
   int i, j, k;

   c = Matrix(1,npar,1,npar);
   b = Matrix(1,npar,1,1);
   func = Vector(1,npar);

   if (iopt == 0)
      for (i=1; i<=n; i++) sigmy[i] = 1e0;        // iopt = 0: equall weights

   for (i=1; i<=npar; i++) {                                // initialization
      for (j=1; j<=npar; j++) c[i][j] = 0e0;
      b[i][1] = 0e0;
   }

   for (i=1; i<=n; i++) {                     // generate the system matrices
      yi = y[i];
      Func(x[i],func,npar);                   // evaluate the basis functions
      f = 1e0/(sigmy[i]*sigmy[i]);
      for (j=1; j<=npar; j++) {
         fj = f * func[j];
         for (k=1; k<=npar; k++) c[j][k] += fj * func[k];
         b[j][1] += fj * yi;
      }
   }

   GaussJordan(c,b,npar,1,det);                           // solve the system

   for (i=1; i<=npar; i++) {
      a[i] = b[i][1];                                     // model parameters
      sigma[i] = sqrt(c[i][i]);                    // parameter uncertainties
   }

   chi2 = 0e0;                                // value of Chi-square function
   for (i=1; i<=n; i++) {
      Func(x[i],func,npar);                    // evaluate the model function
      f = 0e0;
      for (j=1; j<=npar; j++) f += a[j]*func[j];
      f = (y[i] - f)/sigmy[i];
      chi2 += f*f;
   }

   FreeMatrix(c,1,1);
   FreeMatrix(b,1,1);
   FreeVector(func,1);
}

//===========================================================================
void PolFit(double x[], double y[], double sigmy[], int n, int iopt,
            double a[], double sigma[], int npar, double &chi2)
//---------------------------------------------------------------------------
// Determines the coefficients a[i], i=1,..,npar of a polynomial model
//    F(x;a) = a[1] * x^(npar-1) + a[2] * x^(npar-2) + ... + a[npar]
// and the associated uncertainties, sigma[i], for a set of observed data
// points by "Chi-square" regression
// x[]     - x-coordinates of observed data, i = 1,..,n
// y[]     - y-coordinates of observed data
// sigmy[] - standard deviations of observed data
// n       - number of observed data points
// iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
//         - iopt != 0 - uses the received values of sigmy[i]
// a[]     - parameters of the polynomial model (output)
// sigma[] - uncertainties associated with the model parameters (output)
// npar    - number of model parameters (polynomial order + 1)
// chi2    - value of Chi-square merit function for the output parameters
// Calls: GaussJordan (linsys.h)
//---------------------------------------------------------------------------
{
   double det, f, fj, xi, yi;
   double **b, **c, *func;
   int i, j, k;

   c = Matrix(1,npar,1,npar);
   b = Matrix(1,npar,1,1);
   func = Vector(1,npar);

   if (iopt == 0)
      for (i=1; i<=n; i++) sigmy[i] = 1e0;        // iopt = 0: equall weights

   for (i=1; i<=npar; i++) {                                // initialization
      for (j=1; j<=npar; j++) c[i][j] = 0e0;
      b[i][1] = 0e0;
   }

   for (i=1; i<=n; i++) {                     // generate the system matrices
      xi = x[i]; yi = y[i];
      func[npar] = 1e0;                      // basis functions 1, x, x^2,...
      for (j=npar-1; j>=1; j--) func[j] = xi * func[j+1];
      f = 1e0/(sigmy[i]*sigmy[i]);
      for (j=1; j<=npar; j++) {
         fj = f * func[j];
         for (k=1; k<=npar; k++) c[j][k] += fj * func[k];
         b[j][1] += fj * yi;
      }
   }

   GaussJordan(c,b,npar,1,det);                           // solve the system

   for (i=1; i<=npar; i++) {
      a[i] = b[i][1];                                     // model parameters
      sigma[i] = sqrt(c[i][i]);                    // parameter uncertainties
   }

   chi2 = 0e0;                                // value of Chi-square function
   for (i=1; i<=n; i++) {
      xi = x[i];                               // evaluate the model function
      f = a[1];
      for (j=2; j<=npar; j++) f = f*xi + a[j];
      f = (y[i] - f)/sigmy[i];
      chi2 += f*f;
   }

   FreeMatrix(c,1,1);
   FreeMatrix(b,1,1);
   FreeVector(func,1);
}

//===========================================================================
void Deriv(double x, double a[], double dFda[], int npar,
           double Func(double, double [], int))
//---------------------------------------------------------------------------
// Evaluates the derivatives of model function Func(x,a,npar) at point x with
// respect to parameters a[j], j=1,..,npar and returns them in dFda[j]
//---------------------------------------------------------------------------
{
   const double h0 = 1e-4;            // scale factor for the derivation step
   double a0, F0, h;
   int j;

   F0 = Func(x,a,npar);             // function value for original parameters
   for (j=1; j<=npar; j++) {
      h = a[j] ? h0 * fabs(a[j]) : h0;                     // derivation step
      a0 = a[j];                                            // save parameter
      a[j] += h;                                       // increment parameter
      dFda[j] = (Func(x,a,npar) - F0)/h;                        // derivative
      a[j] = a0;                                         // restore parameter
   }
}

//===========================================================================
double Chi2(double x[], double y[], double sigmy[], int n,
            double a[], double b[], double **c, int npar,
            double Func(double, double [], int))
//---------------------------------------------------------------------------
// Evaluates the Chi2 merit function and the fitting matrices c and b for the
// model function Func(x,a,npar) and the Levenberg-Marquardt algorithm
// Calls: Deriv
//---------------------------------------------------------------------------
{
   int i, j, k;
   double chi2, dy, *dFda, fact, fsig;

   dFda = Vector(1,npar);                       // model function derivatives

   for (k=1; k<=npar; k++) {                                // initialization
      for (j=1; j<=npar; j++) c[k][j] = 0e0;
      b[k] = 0e0;
   }

   chi2 = 0e0;
   for (i=1; i<=n; i++) {
      dy = y[i] - Func(x[i],a,npar);          // deviation of model from data
      fsig = 1e0/(sigmy[i]*sigmy[i]);
      chi2 += fsig * dy * dy;                      // evaluate merit function

      Deriv(x[i],a,dFda,npar,Func); // derivatives with respect to parameters
      for (k=1; k<=npar; k++) {                     // build fitting matrices
         fact = fsig * dFda[k];
         for (j=1; j<=npar; j++) c[k][j] += fact * dFda[j];
         b[k] += fact * dy;
      }
   }

   FreeVector(dFda,1);
   return chi2;
}

//===========================================================================
void MarqFit(double x[], double y[], double sigmy[], int n, int iopt,
             double a[], double sigma[], int npar, double &chi2,
             double Func(double, double [], int))
//---------------------------------------------------------------------------
// Minimizes the "Chi-square" merit function by the Levenberg-Marquardt
// method, determining the coefficients a[i], i=1,..,npar of a non-linear
// model F(x,a,npar) and the associated uncertainties, sigma[i], for a set of
// observed data points
// x[]     - x-coordinates of the observed data, i=1,..,n
// y[]     - y-coordinates of the observed data
// sigmy[] - standard deviations of the observed data
// n       - number of observed data points
// iopt    - iopt == 0 - initializes sigmy[i] = 1 (equal weights)
//         - iopt != 0 - uses the received values of sigmy[i]
// a[]     - parameters of the non-linear model (output)
// sigma[] - uncertainties of the model parameters (output)
// npar    - number of model parameters
// chi2    - value of Chi-square merit function for the output parameters
// Func()  - user function returning for a given argument x the values of the
//           basis functions func[i](x):
//              void Func(double x, double func[], int npar);
// Calls: GaussJordan (linsys.h)
//---------------------------------------------------------------------------
{
   const double eps = 1e-4;                   // relative precision criterion
   const int itmax = 1000;                          // max. no. of iterations
   double *a1, *b, *b0, **c, **c0, **cov, **da;
   double chi20, det, err, errj, lam;
   int i, it, j, k;

   c   = Matrix(1,npar,1,npar); c0 = Matrix(1,npar,1,npar);
   cov = Matrix(1,npar,1,npar); da = Matrix(1,npar,1,1);
   a1 = Vector(1,npar); b = Vector(1,npar); b0 = Vector(1,npar);

   if (iopt == 0)
      for (i=1; i<=n; i++) sigmy[i] = 1e0;        // iopt = 0: equall weights

   lam = 1e-3;                                    // initial guess for lambda
   chi2 = Chi2(x,y,sigmy,n,a,b,c,npar,Func);

   it = 0;
   while (it < itmax) {                   // loop of parameter approximations
      chi20 = chi2;
      for (k=1; k<=npar; k++) {           // store fitting matrices c0 and b0
         for (j=1; j<=npar; j++) c0[k][j] = c[k][j];
         b0[k] = b[k];
      }
      while (it < itmax) {                         // loop of lambda increase
         it++;
         for (k=1; k<=npar; k++) {                       // covariance matrix
            for (j=1; j<=npar; j++) cov[k][j] = c0[k][j];
            cov[k][k] *= (1e0 + lam);                       // scale diagonal
            da[k][1] = b0[k];
         }
         GaussJordan(cov,da,npar,1,det);          // solution: corrections da
         for (j=1; j<=npar; j++) a1[j] = a[j] + da[j][1];     // trial params
         chi2 = Chi2(x,y,sigmy,n,a1,b,c,npar,Func); // new linearized c and b
         if (chi2 <= chi20) break;                  // stop increasing lambda
         lam *= 10e0;                                      // increase lambda
      }

      err = 0e0;                      // maximum relative error of parameters
      for (j=1; j<=npar; j++) {
         a[j] += da[j][1];                               // update parameters
         errj = a[j] ? fabs(da[j][1]/a[j]) : fabs(da[j][1]);
         if (err < errj) err = errj;
      }
      lam *= 0.1e0;                                          // reduce lambda
                                                         // check convergence
      if ((err <= eps) && (fabs(1e0 - chi2/chi20) <= eps)) break;
   }
   if (it >= itmax) printf("MarqFit: max. # of iterations exceeded !\n");
                                               // uncertainties of parameters
   for (j=1; j<=npar; j++) sigma[j] = sqrt(cov[j][j]);

   FreeMatrix(c  ,1,1); FreeMatrix(c0,1,1);
   FreeMatrix(cov,1,1); FreeMatrix(da,1,1);
   FreeVector(a1,1); FreeVector(b,1); FreeVector(b0,1);
}

#endif
