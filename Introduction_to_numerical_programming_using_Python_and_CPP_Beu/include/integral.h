//------------------------------- integral.h --------------------------------
// Contains 1D and 3D integrators for real functions with real variables.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _INTEGRAL_
#define _INTEGRAL_

#include <stdio.h>
#include <math.h>
#include "memalloc.h"
#include "specfunc.h"

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

//===========================================================================
double qSimpson(double Func(double), double a, double b, int n)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,b] using Simpson's rule with n
// (odd) integration points
//---------------------------------------------------------------------------
{
   double h, s1, s2;
   int i;

   if (n % 2 == 0) n++;                                // increment n if even

   h = (b-a)/(n-1);
   s1 = s2 = 0e0;
   for (i=2; i<=n-3; i+=2) { s1 += Func(a + i*h); }          // odd-index sum
   for (i=1; i<=n-2; i+=2) { s2 += Func(a + i*h); }         // even-index sum

   return (h/3)*(Func(a) + 4*s2 + 2*s1 + Func(b));
}

//===========================================================================
double qTrapzCtrl(double Func(double), double a, double b, double eps)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,b] with relative precision eps
// using the adaptive Trapezoidal Rule
//---------------------------------------------------------------------------
{
   const int kmax = 30;                // max. no. of step halving iterations
   double h, sum, t, t0;
   long i, n;
   int k;

   h = b-a; n = 1;
   t0 = 0.5*h*(Func(a) + Func(b));                   // initial approximation

   for (k=1; k<=kmax; k++) {                             // step halving loop
      sum = 0e0;
      for (i=1; i<=n; i++) sum += Func(a+(i-0.5)*h);
      t = 0.5*(t0 + h*sum);                              // new approximation
      if (k > 1) {                                       // convergence check
         if (fabs(t-t0) <= eps*fabs(t)) break;
         if (fabs(t) <= eps && fabs(t) <= fabs(t-t0)) break; // integral ~= 0
      }
      h *= 0.5; n *= 2;                             // halve integration step
      t0 = t;
   }
   if (k > kmax) printf("qTrapzCtrl: max. no. of iterations exceeded !\n");
   
   return t;
}

//===========================================================================
double qSimpsonCtrl(double Func(double), double a, double b, double eps)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,b] with relative precision eps
// using the adaptive Simpson Rule
//---------------------------------------------------------------------------
{
   const int kmax = 30;                // max. no. of step halving iterations
   double h, s, s0, sum, t, t0;
   long i, n;
   int k;

   h = b-a; n = 1;
   s0 = t0 = 0.5*h*(Func(a) + Func(b));              // initial approximation

   for (k=1; k<=kmax; k++) {                             // step halving loop
      sum = 0e0;
      for (i=1; i<=n; i++) sum += Func(a+(i-0.5)*h);
      t = 0.5*(t0 + h*sum);
      s = (4*t - t0)/3;                                  // new approximation
      if (k > 1) {                                       // convergence check
         if (fabs(s-s0) <= eps*fabs(s)) break;
         if (fabs(s) <= eps && fabs(s) <= fabs(s-s0)) break; // integral ~= 0
      }
      h *= 0.5; n *= 2;                             // halve integration step
      s0 = s; t0 = t;
   }
   if (k > kmax) printf("qSimpsonCtrl: max. no. of iterations exceeded!\n");

   return s;
}

//===========================================================================
double qRomberg(double Func(double), double a, double b, double eps)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,b] with relative precision eps
// using the adaptive Romberg method
//---------------------------------------------------------------------------
{
   const int kmax = 30;                // max. no. of step halving iterations
   double r1[kmax+1];                                // two consecutive lines
   double r2[kmax+1];                                // from the method table
   double f, h, sum;
   long i, n;
   int j, k;

   h = b-a; n = 1;
   r1[0] = 0.5*h*(Func(a) + Func(b));                // initial approximation
   for (k=1; k<=kmax; k++) {                             // step halving loop
      sum = 0e0;
      for (i=1; i<=n; i++) sum += Func(a+(i-0.5)*h);
      r2[0] = 0.5*(r1[0] + h*sum);                       // trapezoid formula
      f = 1e0;
      for (j=1; j<=k; j++) {                     // increase quadrature order
         f *= 4;
         r2[j] = (f*r2[j-1] - r1[j-1])/(f-1);            // new approximation
      }
      if (k > 1) {                                       // convergence check
         if (fabs(r2[k]-r1[k-1]) <= eps*fabs(r2[k])) break;
         if (fabs(r2[k]) <= eps && fabs(r2[k]) <= fabs(r2[k]-r1[k-1])) break;
      }
      h *= 0.5; n *= 2;                             // halve integration step
      for (j=0; j<=k; j++) r1[j] = r2[j];                // shift table lines
   }
   if (k > kmax) {
      printf("qRomberg: max. no. of iterations exceeded !\n");
      k--;
   }

   return r2[k];
}

//===========================================================================
double qImprop1(double Func(double), double a, double &xinf, double eps)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,+inf) (for xinf >= 0) or (-inf,a]
// (for xinf < 0) with relative precision eps. On output, xinf contains the
// integration domain limit for which convergence was achieved.
// Calls: qRomberg
//---------------------------------------------------------------------------
{
   double h, s, s1, x;

   h = 1e0; x = a;  // subinterval length and initial left limit for [a,+inf)
   if (xinf < 0e0) { h = -h; x = a + h; }                     // for (-inf,a]

   s1 = 1e0;
   s = 0e0;
   while(fabs(s1) > eps*fabs(s) || fabs(Func(x)) > eps) {
      s1 = qRomberg(Func,x,x+fabs(h),eps);            // integral for [x,x+h]
      s += s1;                                       // update total integral
      x += h;                                       // shift interval [x,x+h]
   }
   xinf = x;                                     // final "infinite" boundary

   return s;
}

//===========================================================================
double qImprop2(double Func(double), double a, double b, double eps)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,b] with a and/or b singular
// integrable points.
// Calls: qRomberg
//---------------------------------------------------------------------------
{
   double h, h0, s, s1, x;

   h0 = 0.1e0 * (b-a);         // extent of vicinities of singular boundaries

   s = 0e0;
   if (fabs(Func(a)) > 9e99) {                        // a is singular point?
      h = h0;
      s1 = 1e0;
      while(fabs(s1) > eps*fabs(s)) { 
         h *= 0.5;                                          // halve interval
         x = a + h;                               // left boundary of [x,x+h]
         s1 = qRomberg(Func,x,x+h,eps);        // partial integral on [x,x+h]
         s += s1;                       // add contribution to total integral
      }
      a += h0;                          // new left boundary of core interval
   }

   if (fabs(Func(b)) > 9e99) {                        // b is singular point?
      h = h0;
      s1 = 1e0;
      while(fabs(s1) > eps*fabs(s)) {
         h *= 0.5;                                          // halve interval
         x = b - h;                              // right boundary of [x-h,x]
         s1 = qRomberg(Func,x-h,x,eps);        // partial integral on [x-h,x]
         s += s1;                       // add contribution to total integral
      }
      b -= h0;                         // new right boundary of core interval
   }

   s += qRomberg(Func,a,b,eps);              // add integral on core interval

   return s;
}

//===========================================================================
double qMidPoint(double Func(double), double a, double b, double eps)
//---------------------------------------------------------------------------
// Integrates function Func on interval (a,b) with relative precision eps
// using the adaptive midpoint rule
//---------------------------------------------------------------------------
{
   const int kmax = 19;                              // max. no. of divisions
   double h, s, s0, sum;
   double f1p6 = 1e0/6e0, f5p6 = 5e0/6e0;
   long i, n;
   int k;

   h = b-a; n = 1;
   s0 = h * Func(a+0.5*h);                           // initial approximation

   for (k=1; k<=kmax; k++) {                            // step division loop
      sum = 0e0;
      for (i=1; i<=n; i++) sum += Func(a+(i-f5p6)*h) + Func(a+(i-f1p6)*h);
      s = (s0 + h*sum)/3;                                // new approximation
      if (fabs(s - s0) <= eps*fabs(s)) break;            // convergence check
      h /= 3; n *= 3;                                          // reduce step
      s0 = s;
   }
   if (k > kmax) printf("qMidPoint: max. no. of iterations exceeded !\n");

   return s;
}

//===========================================================================
void xGaussLeg(double a, double b, double x[], double w[], int n)
//---------------------------------------------------------------------------
// Calculates abscissas x[] and weights w[] for the n-point Gauss-Legendre
// quadrature on interval [a,b]
// Calls: Legendre (from specfunc.h)
//---------------------------------------------------------------------------
{
#define pi 3.141592653589793
   const double eps = 1e-14;                   // relative precision of zeros
   double d, f, xc, xi;
   int i, n2;

   n2 = n/2;
   for (i=1; i<=n2; i++) {
      xi = cos(pi*(i-0.25e0)/(n+0.5e0));   // initial approximation for zeros
      f = 9e99;
      while (fabs(f) > eps*fabs(xi)) {           // Newton-Raphson refinement
         f = Legendre(n,xi,d) / d;
         xi -= f;
      }
      x[i] = -xi; x[n-i+1] = xi;                         // symmetrical zeros
      w[i] = w[n-i+1] = 2e0/((1e0-xi*xi)*d*d);               // equal weights
   }

   if (n % 2 == 1) {                                // odd no. of mesh points
      Legendre(n,0e0,d);
      x[n2+1] = 0e0;
      w[n2+1] = 2e0/(d*d);
   }

   f = 0.5e0*(b-a); xc = 0.5e0*(b+a);            // scaling to interval [a,b]
   for (i=1; i<=n; i++) {
      x[i] = f*x[i] + xc;
      w[i] = f*w[i];
   }
}

//===========================================================================
double qGaussLeg(double Func(double), double a, double b, int n)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,b] using n-point Gauss-Legendre
// quadratures
// Calls: xGaussLeg
//---------------------------------------------------------------------------
{
   double s, *x, *w;
   int i;

   x = Vector(1,n);
   w = Vector(1,n);

   xGaussLeg(a,b,x,w,n);

   s = 0e0;
   for (i=1; i<=n; i++) s += w[i] * Func(x[i]);

   FreeVector(x,1);
   FreeVector(w,1);

   return s;
}

//===========================================================================
void xGaussLag(double a, double x[], double w[], int n)
//---------------------------------------------------------------------------
// Calculates abscissas x[] and weights w[] for the n-point Gauss-Laguerre
// quadrature on interval [a,+inf)
// Calls: Laguerre (from specfunc.h)
// Initial approximation for zeros:
// A. Stroud & D. Secrest, Gaussian Quadrature Formulas, Prentice Hall, 1966.
//---------------------------------------------------------------------------
{
   const double eps = 1e-14;                   // relative precision of zeros
   double d, f, xi;
   int i;

   for (i = 1; i <= n; i++) {
      if (i == 1)       // initial approximation for zeros (Stroud & Secrest)
         xi = 3e0/(1e0+2.4e0*n);                                  // 1st zero
      else if (i == 2)
         xi = 15e0/(1e0+2.5e0*n) + x[1];                          // 2nd zero
      else {
         f = (1e0/i+2.55e0)/1.9e0;                              // recurrence
         xi = (1e0+f)*x[i-1] - f*x[i-2];
      }
      f = 9e99;
      while (fabs(f) > eps*fabs(xi)) {           // Newton-Raphson refinement
         f = Laguerre(n,xi,d) / d;
         xi -= f;
      }
      x[i] = xi;
      w[i] = exp(xi)/(xi*d*d);
   }
             
   for (i = 1; i <= n; i++) x[i] += a;        // scaling to interval [a,+inf)
}

//===========================================================================
double qGaussLag(double Func(double), double a, int n)
//---------------------------------------------------------------------------
// Integrates function Func on interval [a,+inf) using n-point Gauss-Laguerre
// quadratures
// Calls: xGaussLag
//---------------------------------------------------------------------------
{
   double s, *x, *w;
   int i;

   x = Vector(1,n);
   w = Vector(1,n);

   xGaussLag(a,x,w,n);

   s = 0e0;
   for (i=1; i<=n; i++) s += w[i] * Func(x[i]);

   FreeVector(x,1);
   FreeVector(w,1);

   return s;
}

//===========================================================================
double qTrapz3D(double Func(double,double,double),
                double ax, double bx, int nx,
                double ay, double by, int ny,
                double az, double bz, int nz)
//---------------------------------------------------------------------------
// Integrates function Func(x,y,z) in the cuboid [ax,bx] x [ay,by] x [az,bz]
// using the trapezoidal rule with (nx x ny x nz) integration points
//---------------------------------------------------------------------------
{
   double hx, hy, hz, s, sx, sy, wx, wy, wz, x, y, z;
   int i, j, k;
   
   hx = (bx-ax)/(nx-1);
   hy = (by-ay)/(ny-1);
   hz = (bz-az)/(nz-1);

   s = 0e0;
   for (i=1; i<=nx; i++) {
      x = ax + (i-1)*hx; wx = ((i-1)*(i-nx) ? hx : 0.5e0*hx);
      sx = 0e0;
      for (j=1; j<=ny; j++) {
         y = ay + (j-1)*hy; wy = ((j-1)*(j-ny) ? hy : 0.5e0*hy);
         sy = 0e0;
         for (k=1; k<=nz; k++) {
            z = az + (k-1)*hz; wz = ((k-1)*(k-nz) ? hz : 0.5e0*hz);
            sy += wz * Func(x,y,z);
         }
         sx += wy * sy;
      }
      s += wx * sx;
   }
   return s;
}

//===========================================================================
void xSimpson(double a, double b, double x[], double w[], int n)
//---------------------------------------------------------------------------
// Calculates abscissas x[] and weights w[] for Simpson's rule with n
// integration points on interval [a,b]
//---------------------------------------------------------------------------
{
   const double c13 = 1e0/3e0, c23 = 2e0/3e0, c43 = 4e0/3e0;
   double h;
   int i;
   
   if (n % 2 == 0) n++;                                // increment n if even

   h = (b-a)/(n-1);
   for (i=1; i<=n; i++) {
      x[i] = a + (i-1)*h; w[i] = h * (i % 2 ? c23 : c43);
   }
   w[1] = w[n] = h * c13;
}

//===========================================================================
double qSimpson3D(double Func(double,double,double),
                  double ax, double bx, int nx,
                  double ay, double by, int ny,
                  double az, double bz, int nz)
//---------------------------------------------------------------------------
// Integrates function Func(x,y,z) in the cuboid [ax,bx] x [ay,by] x [az,bz]
// using Simpson's rule with (nx x ny x nz) integration points
//---------------------------------------------------------------------------
{
   double *wx, *wy, *wz, *x, *y, *z;
   double s, sx, sy;
   int i, j, k;
   
   if (nx % 2 == 0) nx++;                             // increment nx if even
   if (ny % 2 == 0) ny++;                             // increment ny if even
   if (nz % 2 == 0) nz++;                             // increment nz if even

   x = Vector(1,nx); wx = Vector(1,nx);
   y = Vector(1,ny); wy = Vector(1,ny);
   z = Vector(1,nz); wz = Vector(1,nz);

   xSimpson(ax,bx,x,wx,nx);                    // generate integartion points
   xSimpson(ay,by,y,wy,ny);
   xSimpson(az,bz,z,wz,nz);

   s = 0e0;
   for (i=1; i<=nx; i++) {
      sx = 0e0;
      for (j=1; j<=ny; j++) {
         sy = 0e0;
         for (k=1; k<=nz; k++) {
            sy += wz[k] * Func(x[i],y[j],z[k]);
         }
         sx += wy[j] * sy;
      }
      s += wx[i] * sx;
   }

   FreeVector(x,1); FreeVector(wx,1);
   FreeVector(y,1); FreeVector(wy,1);
   FreeVector(z,1); FreeVector(wz,1);

   return s;
}

//===========================================================================
double qGaussLeg3D(double Func(double,double,double),
                   double ax, double bx, int nx,
                   double ay, double by, int ny,
                   double az, double bz, int nz)
//---------------------------------------------------------------------------
// Integrates function Func(x,y,z) in the cuboid [ax,bx] x [ay,by] x [az,bz]
// using Gauss-Legendre quadratures with (nx x ny x nz) integration points
//---------------------------------------------------------------------------
{
   double *wx, *wy, *wz, *x, *y, *z;
   double s, sx, sy;
   int i, j, k;

   x = Vector(1,nx); wx = Vector(1,nx);
   y = Vector(1,ny); wy = Vector(1,ny);
   z = Vector(1,nz); wz = Vector(1,nz);

   xGaussLeg(ax,bx,x,wx,nx);                   // generate integartion points
   xGaussLeg(ay,by,y,wy,ny);
   xGaussLeg(az,bz,z,wz,nz);

   s = 0e0;
   for (i=1; i<=nx; i++) {
      sx = 0e0;
      for (j=1; j<=ny; j++) {
         sy = 0e0;
         for (k=1; k<=nz; k++) {
            sy += wz[k] * Func(x[i],y[j],z[k]);
         }
         sx += wy[j] * sy;
      }
      s += wx[i] * sx;
   }

   FreeVector(x,1); FreeVector(wx,1);
   FreeVector(y,1); FreeVector(wy,1);
   FreeVector(z,1); FreeVector(wz,1);

   return s;
}

//===========================================================================
double qSimpsonAng(double Func(double,double), int nt, int np)
//---------------------------------------------------------------------------
// Integrates function Func(theta,phi) on [0,pi] x [0,2*pi] in sperical
// coordinates using Simpson's rule with (nt x np) points
//---------------------------------------------------------------------------
{
#define pi 3.141592653589793
   double *wt, *wp, *t, *p;
   double s, st;
   int i, j;
   
   if (nt % 2 == 0) nt++;                             // increment nt if even
   if (np % 2 == 0) np++;                             // increment np if even

   t = Vector(1,nt); wt = Vector(1,nt);
   p = Vector(1,np); wp = Vector(1,np);

   xSimpson(0e0,pi,t,wt,nt);                   // generate integartion points
   xSimpson(0e0,2e0*pi,p,wp,np);

   for (i=1; i<=nt; i++) { wt[i] *= sin(t[i]); }            // volume element

   s = 0e0;
   for (i=1; i<=nt; i++) {
      st = 0e0;
      for (j=1; j<=np; j++) {
         st += wp[j] * Func(t[i],p[j]);
      }
      s += wt[i] * st;
   }

   FreeVector(t,1); FreeVector(wt,1);
   FreeVector(p,1); FreeVector(wp,1);

   return s;
}

//===========================================================================
double qSimpsonSph(double Func(double,double,double), double a,
                   int nr, int nt, int np)
//---------------------------------------------------------------------------
// Integrates function Func(r,theta,phi) on [0,a] x [0,pi] x [0,2*pi]
// in sperical coordinates using Simpson's rule with (nr x nt x np) points
//---------------------------------------------------------------------------
{
#define pi 3.141592653589793
   double *wr, *wt, *wp, *r, *t, *p;
   double s, sr, st;
   int i, j, k;
   
   if (nr % 2 == 0) nr++;                             // increment nr if even
   if (nt % 2 == 0) nt++;                             // increment nt if even
   if (np % 2 == 0) np++;                             // increment np if even

   r = Vector(1,nr); wr = Vector(1,nr);
   t = Vector(1,nt); wt = Vector(1,nt);
   p = Vector(1,np); wp = Vector(1,np);

   xSimpson(0e0,a,r,wr,nr);                    // generate integartion points
   xSimpson(0e0,pi,t,wt,nt);
   xSimpson(0e0,2e0*pi,p,wp,np);

   for (i=1; i<=nr; i++) { wr[i] *= r[i] * r[i]; }          // factors from
   for (j=1; j<=nt; j++) { wt[j] *= sin(t[j]); }            // volume element

   s = 0e0;
   for (i=1; i<=nr; i++) {
      sr = 0e0;
      for (j=1; j<=nt; j++) {
         st = 0e0;
         for (k=1; k<=np; k++) {
            st += wp[k] * Func(r[i],t[j],p[k]);
         }
         sr += wt[j] * st;
      }
      s += wr[i] * sr;
   }

   FreeVector(r,1); FreeVector(wr,1);
   FreeVector(t,1); FreeVector(wt,1);
   FreeVector(p,1); FreeVector(wp,1);

   return s;
}

//===========================================================================
double qGaussSph(double Func(double,double,double), int nr, int nt, int np)
//---------------------------------------------------------------------------
// Integrates function Func(r,theta,phi) on [0,inf] x [0,pi] x [0,2*pi]
// in spherical coordinates using Gauss-Laguerre and Gauss-Legendre formulas
// with (nr x nt x np) points
//---------------------------------------------------------------------------
{
#define pi 3.141592653589793
   double *wr, *wt, *wp, *r, *t, *p;
   double s, sr, st;
   int i, j, k;

   r = Vector(1,nr); wr = Vector(1,nr);
   t = Vector(1,nt); wt = Vector(1,nt);
   p = Vector(1,np); wp = Vector(1,np);

   xGaussLag(0e0,r,wr,nr);              // Gauss-Laguerre radial quadrature
   xGaussLeg(0e0,pi,t,wt,nt);           // Gauss-Legendre angular quadratures
   xGaussLeg(0e0,2e0*pi,p,wp,np);

   for (i=1; i<=nr; i++) { wr[i] *= r[i] * r[i]; }          // factors from
   for (j=1; j<=nt; j++) { wt[j] *= sin(t[j]); }            // volume element

   s = 0e0;
   for (i=1; i<=nr; i++) {
      sr = 0e0;
      for (j=1; j<=nt; j++) {
         st = 0e0;
         for (k=1; k<=np; k++) {
            st += wp[k] * Func(r[i],t[j],p[k]);
         }
         sr += wt[j] * st;
      }
      s += wr[i] * sr;
   }

   FreeVector(r,1); FreeVector(wr,1);
   FreeVector(t,1); FreeVector(wt,1);
   FreeVector(p,1); FreeVector(wp,1);

   return s;
}

//===========================================================================
double qSimpsonCyl(double Func(double,double,double),
                   double a, double az, double bz, int nr, int np, int nz)
//---------------------------------------------------------------------------
// Integrates function Func(r,phi,z) on domain [0,a] x [0,2*pi] x [az,bz]
// in cylindrical coordinates using Simpson's rule with (nr x np x nz) points
//---------------------------------------------------------------------------
{
#define pi 3.141592653589793
   double *wr, *wp, *wz, *r, *p, *z;
   double s, sr, sp;
   int i, j, k;
   
   if (nr % 2 == 0) nr++;                             // increment nr if even
   if (np % 2 == 0) np++;                             // increment np if even
   if (nz % 2 == 0) nz++;                             // increment nz if even

   r = Vector(1,nr); wr = Vector(1,nr);
   p = Vector(1,np); wp = Vector(1,np);
   z = Vector(1,nz); wz = Vector(1,nz);

   xSimpson(0e0,a,r,wr,nr);                    // generate integartion points
   xSimpson(0e0,2e0*pi,p,wp,np);
   xSimpson(az,bz,z,wz,nz);

   for (i=1; i<=nr; i++) { wr[i] *= r[i]; }     // factor from volume element

   s = 0e0;
   for (i=1; i<=nr; i++) {
      sr = 0e0;
      for (j=1; j<=np; j++) {
         sp = 0e0;
         for (k=1; k<=nz; k++) {
            sp += wz[k] * Func(r[i],p[j],z[k]);
         }
         sr += wp[j] * sp;
      }
      s += wr[i] * sr;
   }

   FreeVector(r,1); FreeVector(wr,1);
   FreeVector(p,1); FreeVector(wp,1);
   FreeVector(z,1); FreeVector(wz,1);

   return s;
}

#endif
