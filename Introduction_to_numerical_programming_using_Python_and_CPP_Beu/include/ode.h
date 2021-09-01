//---------------------------------- ode.h ----------------------------------
// Contains routines for solving systems of ordinary differential equations.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _ODE_
#define _ODE_

#include <math.h>
#include "memalloc.h"
#include "linsys.h"

//===========================================================================
void Euler(double t, double ht, double y[], int n,
           void Func(double,double[],double[]))
//---------------------------------------------------------------------------
// Propagates the solution y of a system of 1st order ODEs
//    y'[i] = f[i](t,y[]), i = 1..n
// from t to t+ht using Euler's method
// Calls: Vector (memalloc.h)
//        void Func(double t, double y[], double f[]) - RHS of ODEs
//---------------------------------------------------------------------------
{
   static double *f;
   static int init = 1;                              // initialization switch
   int i;

   if (init) { f = Vector(1,n); init = 0; }           // 1st entry allocation

   Func(t,y,f);                                            // get RHS of ODEs
   for (i=1; i<=n; i++) y[i] += ht * f[i];              // propagate solution
}

//===========================================================================
void EulerPC(double t, double ht, double y[], int n,
             void Func(double,double[],double[]))
//---------------------------------------------------------------------------
// Propagates the solution y of a system of 1st order ODEs
//    y'[i] = f[i](t,y[]), i = 1..n
// from t to t+ht using Euler's predictor-corrector method
// Calls: Vector (memalloc.h)
//        void Func(double t, double y[], double f[]) - RHS of ODEs
//---------------------------------------------------------------------------
{
   static double *f1, *f2, *yt;
   static int init = 1;                              // initialization switch
   double ht2;
   int i;

   if (init) {                                        // 1st entry allocation
      f1 = Vector(1,n); f2 = Vector(1,n);                      // RHS of ODEs
      yt = Vector(1,n);                                 // predicted solution
      init = 0;
   }

   Func(t,y,f1);                                          // RHS of ODEs at t
   for (i=1; i<=n; i++) yt[i] = y[i] + ht * f1[i];               // predictor
   Func(t+ht,yt,f2);                                   // RHS of ODEs at t+ht

   ht2 = ht/2e0;
   for (i=1; i<=n; i++) y[i] += ht2 * (f1[i] + f2[i]);           // corrector
}

//===========================================================================
void RungeKutta(double t, double ht, double y[], int n,
                void Func(double,double[],double[]))
//---------------------------------------------------------------------------
// Propagates the solution y of a system of 1st order ODEs
//    y'[i] = f[i](t,y[]), i = 1..n
// from t to t+ht using the 4th order Runge-Kutta method
// Calls: Vector (memalloc.h)
//        void Func(double t, double y[], double f[]) - RHS of ODEs
//---------------------------------------------------------------------------
{
   static double *f1, *f2, *f3, *f4, *yt;
   static int init = 1;                              // initialization switch
   double ht2, ht6;
   int i;

   if (init) {                                        // 1st entry allocation
      f1 = Vector(1,n); f2 = Vector(1,n); f3 = Vector(1,n); f4 = Vector(1,n);
      yt = Vector(1,n);                                 // predicted solution
      init = 0;
   }

   ht2 = ht/2e0;
   Func(t,y,f1);                                                  // RHS at t
   for (i=1; i<=n; i++) yt[i] = y[i] + ht2*f1[i];
   Func(t+ht2,yt,f2);                                        // RHS at t+ht/2
   for (i=1; i<=n; i++) yt[i] = y[i] + ht2*f2[i];
   Func(t+ht2,yt,f3);                                        // RHS at t+ht/2
   for (i=1; i<=n; i++) yt[i] = y[i] + ht *f3[i];
   Func(t+ht,yt,f4);                                           // RHS at t+ht

   ht6 = ht/6e0;                                        // propagate solution
   for (i=1; i<=n; i++) y[i] += ht6*(f1[i] + 2*(f2[i] + f3[i]) + f4[i]);
}

//===========================================================================
void RKadapt(double t, double &ht, double &ht1, double eps,
             double y[], int n, void Func(double,double[],double[]))
//---------------------------------------------------------------------------
// Propagates the solution y of a system of 1st order ODEs
//    y'[i] = f[i](t,y[]), i = 1..n
// from t to t+ht using 4th order Runge-Kutta and adaptive step size control
//
// ht   - initial step size (input); final step size (output):
//        ht is unchanged if err <= eps; ht is reduced if err > eps
// ht1  - step size guess for next propagation (output)
// eps  - max. relative error of solution components
// Calls: Vector (memalloc.h)
//        void Func(double t, double y[], double f[]) - RHS of ODEs
//---------------------------------------------------------------------------
{
   const int p = 4;                              // order of basic ODE solver
   const int itmax = 10;                  // max. no. of step size reductions
   static double *yt, *yt2;            // trial solutions for t+ht and t+ht/2
   static int init = 1;                              // initialization switch
   double err, erri, f, ht2;
   int i, it;
                                                      // 1st entry allocation
   if (init) { yt = Vector(1,n); yt2 = Vector(1,n); init = 0; }

   for (it=1; it<=itmax; it++) {                 // loop of step size scaling
      ht2 = ht/2e0;
      for (i=1; i<=n; i++) yt2[i] = yt[i] = y[i];    // initialize trial sol.
      RungeKutta(t,ht,yt,n,Func);                                // t -> t+ht
      RungeKutta(t,ht2,yt2,n,Func);                            // t -> t+ht/2
      RungeKutta(t+ht2,ht2,yt2,n,Func);                     // t+ht/2 -> t+ht

      err = 0e0;                         // max. error of solution components
      for (i=1; i<=n; i++) {
         erri = yt2[i] ? fabs(1e0 - yt[i]/yt2[i]) : fabs(yt2[i] - yt[i]);
         if (err < erri) err = erri;
      }

      f = 1e0;                                // scaling factor for step size
      if (err) f = 0.9e0*pow(eps/err,1e0/p);
      if (f > 5e0) f = 5e0;                          // prevent increase > 5x
      ht1 = f * ht;                               // guess for next step size
      if (err <= eps) break;                      // precision attained: exit
      ht = ht1;                                // reduce step size and repeat
   }
   if (it > itmax) printf("RKadapt: max. no. of iterations exceeded !\n");
   for (i=1; i<=n; i++) y[i] = yt2[i];     // update y with the best solution
}

//===========================================================================
void RKFehlberg(double t, double &ht, double &ht1, double eps,
                double y[], int n, void Func(double,double[],double[]))
//---------------------------------------------------------------------------
// Propagates the solution y of a system of 1st order ODEs
//    y'[i] = f[i](t,y[]), i = 1..n
// from t to t+ht using the Runge-Kutta-Fehlberg method with stepsize control
//
// ht   - initial step size (input); final step size (output):
//        ht is unchanged if err <= eps; ht is reduced if err > eps
// ht1  - step size guess for next propagation (output)
// eps  - max. relative error of solution components
// Calls: Vector (memalloc.h)
//        void Func(double t, double y[], double f[]) - RHS of ODEs
//---------------------------------------------------------------------------
{
   const int p = 5;                              // order of basic ODE solver
   const int itmax = 10;                  // max. no. of step size reductions
   const double                          // Runge-Kutta-Fehlberg coefficients
      a2 = 1e0/4e0, a3 = 3e0/8e0, a4 = 12e0/13e0, a5 = 1e0, a6 = 1e0/2e0,
      b21 = 1e0/4e0, b31 = 3e0/32e0, b32 = 9e0/32e0,
      b41 = 1932e0/2197e0, b42 = -7200e0/2197e0, b43 = 7296e0/2197e0,
      b51 = 439e0/216e0, b52 = -8e0, b53 = 3680e0/513e0, b54 = -845e0/4104e0,
      b61 = -8e0/27e0, b62 = 2e0, b63 = -3544e0/2565e0, b64 = 1859e0/4104e0,
      b65 = -11e0/40e0,
      c1 = 16e0/135e0, c3 = 6656e0/12825e0, c4 = 28561e0/56430e0,
      c5 = -9e0/50e0, c6 = 2e0/55e0,
      e1 = 1e0/360e0, e3 = -128e0/4275e0, e4 = -2197e0/75240e0,
      e5 = 1e0/50e0, e6 = 2e0/55e0;
   static double *f1, *f2, *f3, *f4, *f5, *f6, *yt;
   static int init = 1;                              // initialization switch
   double err, erri, f;
   int i, it;

   if (init) {                                        // 1st entry allocation
      f1 = Vector(1,n); f2 = Vector(1,n); f3 = Vector(1,n); f4 = Vector(1,n);
      f5 = Vector(1,n); f6 = Vector(1,n); yt = Vector(1,n);
      init = 0;
   }

   for (it=1; it<=itmax; it++) {                 // loop of step size scaling
      Func(t,y,f1);
      for (i=1; i<=n; i++)
         yt[i] = y[i] + ht*b21*f1[i];
      Func(t+a2*ht,yt,f2);
      for (i=1; i<=n; i++)
         yt[i] = y[i] + ht*(b31*f1[i] + b32*f2[i]);
      Func(t+a3*ht,yt,f3);
      for (i=1; i<=n; i++)
         yt[i] = y[i] + ht*(b41*f1[i] + b42*f2[i] + b43*f3[i]);
      Func(t+a4*ht,yt,f4);
      for (i=1; i<=n; i++)
         yt[i] = y[i] + ht*(b51*f1[i] + b52*f2[i] + b53*f3[i] + b54*f4[i]);
      Func(t+a5*ht,yt,f5);
      for (i=1; i<=n; i++)
         yt[i] = y[i] + ht*(b61*f1[i] + b62*f2[i] + b63*f3[i] + b64*f4[i] +
                            b65*f5[i]);
      Func(t+a6*ht,yt,f6);

      err = 0e0;                         // max. error of solution components
      for (i=1; i<=n; i++) {                       // O(h5) solution estimate
         yt[i] = y[i] +
                 ht*(c1*f1[i] + c3*f3[i] + c4*f4[i] + c5*f5[i] + c6*f6[i]);
                                                            // error estimate
         erri = ht*(e1*f1[i] + e3*f3[i] + e4*f4[i] + e5*f5[i] + e6*f6[i]);
         erri = fabs(erri/yt[i]);
         if (err < erri) err = erri;
      }

      f = 1e0;                                // scaling factor for step size
      if (err) f = 0.9e0*pow(eps/err,1e0/p);
      if (f > 5e0) f = 5e0;                          // prevent increase > 5x
      ht1 = f * ht;                               // guess for next step size
      if (err <= eps) break;                      // precision attained: exit
      ht = ht1;                                // reduce step size and repeat
   }

   if (it > itmax) printf("RKFehlberg: max. no. of iterations exceeded !\n");
   for (i=1; i<=n; i++) y[i] = yt[i];     // update y with the O(h5) solution
}

//===========================================================================
void Euler1(double t, double ht, double &y, double &dy,
            double Func(double,double,double))
//---------------------------------------------------------------------------
// Propagates the solution y and 1st derivative dy of a 2nd order ODE from t
// to t+ht using Euler's method
//---------------------------------------------------------------------------
{
   double d2y;

   d2y = Func(t,y,dy);                                            // d2y -> t
   y  += ht * dy;                                                // y -> t+ht
   dy += ht * d2y;                                              // dy -> t+ht
}

//===========================================================================
void EulerCromer1(double t, double ht, double &y, double &dy,
                  double Func(double,double,double))
//---------------------------------------------------------------------------
// Propagates the solution y and the 1st derivative dy of a 2nd order ODE
// from t to t+ht using the Euler-Cromer method
//---------------------------------------------------------------------------
{
   double d2y;

   d2y = Func(t,y,dy);                                            // d2y -> t
   dy += ht * d2y;                                              // dy -> t+ht
   y  += ht * dy;                                                // y -> t+ht
}

//===========================================================================
void Verlet1(double t, double ht, double &y, double &dy, double &d2y,
             double Func(double,double,double))
//---------------------------------------------------------------------------
// Propagates the solution y and the 1st derivative dy of a 2nd order ODE
// from t to t+ht using the Verlet method; returns 2nd derivative in d2y;
// d2y needs to be initialized on first call and saved between calls
//---------------------------------------------------------------------------
{
   double ht2;

   ht2 = 0.5e0 * ht;
   dy += ht2 * d2y;                                           // dy -> t+ht/2
   y  += ht  * dy;                                               // y -> t+ht

   d2y = Func(t,y,dy);                                          // d2y -> t+h

   dy += ht2 * d2y;                                             // dy -> t+ht
}

//===========================================================================
void Euler2(double t, double ht, double y[], double dy[], int n,
            void Func(double,double[],double[],double[]))
//---------------------------------------------------------------------------
// Propagates the solution y and the 1st derivative dy of a system of n
// 2nd order ODEs from t to t+ht using the Euler method
//---------------------------------------------------------------------------
{
   static double *d2y;
   static int init = 1;                              // initialization switch
   int i;

   if (init) { d2y = Vector(1,n); init = 0; }         // 1st entry allocation

   Func(t,y,dy,d2y);                                              // d2y -> t

   for (i=1; i<=n; i++) {                               // propagate solution
      y[i]  += ht * dy[i];                                       // y -> t+ht
      dy[i] += ht * d2y[i];                                     // dy -> t+ht
   }
}

//===========================================================================
void EulerCromer(double t, double ht, double y[], double dy[], int n,
                 void Func(double,double[],double[],double[]))
//---------------------------------------------------------------------------
// Propagates the solution y and the 1st derivative dy of a system of n
// 2nd order ODEs from t to t+ht using the Euler-Cromer method
//---------------------------------------------------------------------------
{
   static double *d2y;
   static int init = 1;                              // initialization switch
   int i;

   if (init) { d2y = Vector(1,n); init = 0; }         // 1st entry allocation

   Func(t,y,dy,d2y);                                              // d2y -> t

   for (i=1; i<=n; i++) {                               // propagate solution
      dy[i] += ht * d2y[i];                                     // dy -> t+ht
      y[i]  += ht * dy[i];                                       // y -> t+ht
   }
}

//===========================================================================
void Verlet2(double ht, double m,
             double &x, double &y, double &vx, double &vy,
             double &ax, double &ay, double &Ekin, double &Epot,
             void Forces(double,double,double,double,double,
                         double&,double&,double&))
//---------------------------------------------------------------------------
// Propagates the 2D solution of Newton's equations of motion for a particle
// of mass m over a time interval ht using the velocity Verlet method
// x, y   - position components
// vx, vy - velocity components
// ax, ay - acceleration components (need to be initialized on 1st call)
// Ekin   - kinetic energy
// Epot   - potential energy
//---------------------------------------------------------------------------
{
   double fx, fy, ht2;

   ht2 = 0.5e0 * ht;
   vx += ht2 * ax; x += ht * vx;                               // v -> t+ht/2
   vy += ht2 * ay; y += ht * vy;                                 // r -> t+ht

   Forces(m,x,y,vx,vy,fx,fy,Epot);                                  // forces

   ax = fx/m; ay = fy/m;                                         // a -> t+ht
   vx += ht2 * ax;                                               // v -> t+ht
   vy += ht2 * ay;

   Ekin = 0.5e0 * m * (vx*vx + vy*vy);                      // kinetic energy
}

//===========================================================================
void Verlet(double ht, double m[],
            double x[], double y[], double z[],
            double vx[], double vy[], double vz[],
            double ax[], double ay[], double az[], int n,
            double &Ekin, double &Epot,
            void Forces(double[],double[],double[],double[],double[],
                        double[],double[],int,double&))
//---------------------------------------------------------------------------
// Propagates the solution of Newton's equations of motion for a system of n
// particles over a time interval ht using the velocity Verlet method
// m          - masses of particles
// x, y, z    - positions
// vx, vy, vz - velocities
// ax, ay, az - accelerations (need to be initialized on 1st call)
// Ekin       - total kinetic energy
// Epot       - total potential energy
//---------------------------------------------------------------------------
{
   double ht2;
   int i;

   ht2 = 0.5e0 * ht;
   for (i=1; i<=n; i++) {
      vx[i] += ht2 * ax[i]; x[i] += ht * vx[i];                // v -> t+ht/2
      vy[i] += ht2 * ay[i]; y[i] += ht * vy[i];                  // r -> t+ht
      vz[i] += ht2 * az[i]; z[i] += ht * vz[i];
   }

   Forces(m,x,y,z,ax,ay,az,n,Epot);                                 // forces

   Ekin = 0e0;
   for (i=1; i<=n; i++) {
      ax[i] /= m[i]; ay[i] /= m[i]; az[i] /= m[i];               // a -> t+ht
      vx[i] += ht2 * ax[i];                                      // v -> t+ht
      vy[i] += ht2 * ay[i];
      vz[i] += ht2 * az[i];

      Ekin += 0.5 * m[i] * (vx[i]*vx[i] + vy[i]*vy[i] + vz[i]*vz[i]);
   }
}

//===========================================================================
int EulerCromerQM(double E, double V[], double x[], double y[], int nx,
                  double y0, double dy0)
//---------------------------------------------------------------------------
// Propagates the solution of the dimensionless 1D Schrodinger equation
//    y" = 2 [V(x) - E] y, y(0) = y0, y'(0) = y'0
// on a regular mesh x[] with nx points by the Euler-Cromer method. Receives
// the energy in E, the tabulated potential in V[], the initial conditions in
// y0 and dy0. Returns the index of the divergence point (default is nx).
//---------------------------------------------------------------------------
{
   double dy, d2y, hx;
   int m;

   hx = x[2] - x[1];                                 // propagation step size
   y[1] = y0; dy = dy0;                                     // initial values
   for (m=1; m<=nx-1; m++) {                              // propagation loop
      d2y = 2e0 * (V[m] - E) * y[m];          // RHS of Schroedinger equation
      dy += hx * d2y;                                        // dy -> x[m]+hx
      y[m+1] = y[m] + hx * dy;                                // y -> x[m]+hx

      if (fabs(y[m+1]) > 1e10) break;                   // stop if y diverges
   }
   return m;                                     // index of divergence point
}

//===========================================================================
int Numerov(double E, double V[], double x[], double y[], int nx,
            double y0, double dy0)
//---------------------------------------------------------------------------
// Propagates the solution of the dimensionless 1D Schrodinger equation
//    y" = 2 [V(x) - E] y, y(0) = y0, y'(0) = y'0
// on a regular mesh x[] with nx points by the Numerov method. Receives the
// energy in E, the tabulated potential in V[], and the initial conditions in
// y0 and dy0. Returns the index of the divergence point (default is nx).
//---------------------------------------------------------------------------
{
   double dy, d2y, hx, h6, um, um1, up1;
   int m;

   hx = x[2] - x[1];                                 // propagation step size
   y[1] = y0; dy = dy0;                                     // initial values

   d2y = 2e0 * (V[1] - E) * y[1];                // initial Euler-Cromer step
   dy += hx * d2y;
   y[2] = y[1] + hx * dy;

   h6 = hx*hx/6e0;
   um1 = 1e0 - h6 * (V[1] - E);                  // stack of auxiliary values
   um  = 1e0 - h6 * (V[2] - E);
   for (m=2; m<=nx-1; m++) {                              // propagation loop
      up1 = 1e0 - h6 * (V[m+1] - E);
      y[m+1] = ((12e0 - 10e0*um) * y[m] - um1 * y[m-1]) / up1;
      um1 = um; um = up1;                   // shift stack down for next step

      if (fabs(y[m+1]) > 1e10) break;                   // stop if y diverges
   }
   return m;                                     // index of divergence point
}

//===========================================================================
void Propag(double x[], double y[], int nx, double y0, double dy0,
            double Func(double,double,double))
//---------------------------------------------------------------------------
// Propagates the solution y[] of a Cauchy-problem for a 2nd order ODE on a
// regular mesh x[] with nx points, starting from y0 and dy0.
// Calls: EulerCromer1(x, hx, y, dy, Func); Func(x, y, dy) - RHS of ODE
//---------------------------------------------------------------------------
{
   double dy, hx;
   int m;

   hx = x[2] - x[1];                                 // propagation step size
   y[1] = y0; dy = dy0;                                     // initial values
   for (m=1; m<nx; m++)                                   // propagation loop
      { y[m+1] = y[m]; EulerCromer1(x[m],hx,y[m+1],dy,Func); }
}

//===========================================================================
double Shoot(double x[], double y[], int nx, double ya, double yb,
             double dy1, double dy2, double eps, int &exist,
             double Func(double,double,double))
//---------------------------------------------------------------------------
// Solves a two-point boundary-value problem for a 2nd order ODE
//    y" = f(x,y,y'), y(xa) = ya, y(xb) = yb
// on a regular mesh x[] with nx points, using the shooting method with trial
// initial derivatives dy in [dy1,dy2]. Returns the solution y[] satisfying
// the condition y(xb) = yb within tolerance eps, the used derivative dy, and
// an existence flag.
// Calls: Propag(x, y, nx, y0, dy0, Func); Func(x, y, dy) - RHS of ODE
//---------------------------------------------------------------------------
{
   const int itmax = 100;                           // max. no. of bisections
   double dy, f, f1, f2;
   int it;

   Propag(x,y,nx,ya,dy1,Func);                          //propagate y for dy1
   f1 = y[nx] - yb;                                         //deviation at xb
   Propag(x,y,nx,ya,dy2,Func);                          //propagate y for dy2
   f2 = y[nx] - yb;                                         //deviation at xb

   if (f1*f2 < 0) {                         //check if dy exists in [dy1,dy2]
      exist = 1;
      for (it=1; it<=itmax; it++) {                  //refine dy by bisection
         dy = 0.5e0*(dy1 + dy2);                          //new approximation
         Propag(x,y,nx,ya,dy,Func);                             //propagate y
         f = y[nx] - yb;                                    //deviation at xb
         if (f1*f > 0) dy1 = dy; else dy2 = dy;           //new semi interval
         if (fabs(f) <= eps) break;              //deviation vanishes at xb ?
      }
      if (it >= itmax) printf("Shoot: max. no. of bisections exceeded !\n");
   } else
      { dy = 1e10; exist = 0; }
   return dy;
}

//===========================================================================
double ShootQM(double E1, double E2, double V[], double x[], double y[],
               int nx, int nc, double y0, double dy0, double eps, int &exist)
//---------------------------------------------------------------------------
// Solves the two-point eigenvalue problem for the 1D Schrodinger equation
//    y" = (2m/h^2) [V(x) - E] y, y(0) = y0, y(+inf) = 0
// on a regular mesh x[] with nx points, using the shooting method with the
// trial energies E in [E1,E2] and the tabulated potential in V[]. Returns
// the solution y[] vanishing at the checkpoint x[nc] within tolerance eps,
// and the existence flag exist.
// Calls: Numerov(V, x, y, nx, y0, dy0, E)
//---------------------------------------------------------------------------
{
   const int itmax = 100;                           // max. no. of bisections
   double E, f, f1, f2;
   int inf, it;

   inf = Numerov(E1,V,x,y,nx,y0,dy0);                   // propagate y for E1
   f1 = y[inf];                                               // asymptotic y
   inf = Numerov(E2,V,x,y,nx,y0,dy0);                   // propagate y for E2
   f2 = y[inf];                                               // asymptotic y

   if (f1*f2 < 0) {                            //check if exists E in [E1,E2]
      exist = 1;
      for (it=1; it<=itmax; it++) {                   //refine E by bisection
         E = 0.5e0*(E1 + E2);                             //new approximation
         inf = Numerov(E,V,x,y,nx,y0,dy0);                      //propagate y
         f = y[inf];                                          // asymptotic y
         if (f1*f > 0) E1 = E; else E2 = E;               //new semi interval
         if (fabs(y[nc]) <= eps) break;      //check if y vanishes at x[nc] ?
      }
      if (it >= itmax) printf("ShootQM: max. no. of bisections exceeded!\n");
   } else
      { E = 1e10; exist = 0; }
   return E;
}

//===========================================================================
void Bilocal(double xa, double xb, double y[], int nx,
             double alf1, double bet1, double alf2, double bet2,
             void Func(double, double&, double&, double&))
//---------------------------------------------------------------------------
// Solves a linear two-point boundary-value problem for a 2nd order ODE
//    y" = p(x) y' + q(x) y + r(x),  xa <= x <= xb
//    alf1 y(xa) + bet1 y'(xa) = 1
//    alf2 y(xa) + bet2 y'(xa) = 1
// on a regular mesh with nx points, using central finite-differences.
// Returns the solution in y[].
// Calls: Func(x, p, q, r) - returns values of p(x), q(x), r(x)
//        TriDiagSys (linsys.h) - solves discretized tridiagonal system
//---------------------------------------------------------------------------
{
   double *a, *b, *c;
   double hx, h2, p, q, r, x;
   int m;

   a = Vector(1,nx); b = Vector(1,nx); c = Vector(1,nx);   // matrix elements

   hx = (xb-xa)/(nx-1); h2 = 2e0*hx*hx;
                                                 // build system coefficients
   b[1] = hx*alf1 - bet1; c[1] = bet1; y[1] = hx;
   for (m=2; m<=nx-1; m++) {
      x = xa + (m-1)*hx;                                        // mesh point
      Func(x,p,q,r);
      a[m] = -(2e0 + hx*p); b[m] = 4e0 + h2*q; c[m] = -(2e0 - hx*p);
      y[m] = -h2*r;                             // RHSs of tridiagonal system
   }
   a[nx] = -bet2; b[nx] = hx*alf2 + bet2; y[nx] = hx;

   TriDiagSys(a,b,c,y,nx);                        // solve discretized system

   FreeVector(a,1); FreeVector(b,1); FreeVector(c,1);
}

#endif