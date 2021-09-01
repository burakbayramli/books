// Planck's law of black body radiation
#include <math.h>
#include "memalloc.h"
#include "roots.h"
#include "graphlib.h"

#define pi 3.141592653589793
#define hP 6.62606957e-34                          // Planck's constant (J s)
#define kB 1.3806488e-23                          // Boltzmann constant (J/K)
#define c  2.99792458e8                               // speed of light (m/s)

double lam0;                                          // reference wavelength
double K;                                                 // factor of u(lam)

double u(double lam)                        // spectral energy density u(lam)
{
   double x;
   if (lam == 0e0) return 0e0;
   x = lam / lam0;
   return K / (pow(x,5) * (exp(1e0/x) - 1e0));
}
double du(double lam)                                              // du/dlam
{
   double expx, exp1, x;
   if (lam == 0e0) return 0e0;
   x = lam / lam0;
   expx = exp(1e0/x);
   exp1 = expx - 1e0;
   return K * lam0 * (5e0*x + (1e0-5e0*x)*expx) / (pow(x,7) * exp1*exp1);
}

int main(int argc, wchar_t** argv)
{
   int i, ierr, j, n, nmax; 
   double f, fmax, fmin, h, lam, lam_1, lam_2, lam_max, lam_plot, T;
   double *x, *y, *z;
   int sty[4], nn[4];
   const char* col[4]; 

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   lam_plot = 3e-6;                           // maximum wavelength for plots
   h = 1e-8;                                      // step-size for wavelength
   n = int(lam_plot/h + 0.5) + 1;                  // number of points / plot
   nmax = 3 * n;                                    // total number of points

   x = Vector(1,nmax); y = Vector(1,nmax); z = Vector(1,nmax);

   for (j=0; j<=2; j++) {
      T = 4000e0 + j * 1000e0;                         // current temperature
      lam0 = hP*c / (kB*T);                           // reference wavelength
      K = 8e0 * pi * kB * T / pow(lam0,4);                // factor of u(lam)
      for (i=1; i<=n; i++) {
         lam = (i-1)*h;
         x[i+j*n] = lam * 1e6;                                     // microns
         y[i+j*n] = u(lam);
         z[i+j*n] = du(lam);
      }
   }
   fmax = 0e0;                                          // normalize profiles
   for (i=1; i<=nmax; i++) if (y[i] > fmax) fmax = y[i];
   for (i=1; i<=nmax; i++) y[i] /= fmax;
   fmax = 0e0;
   for (i=1; i<=nmax; i++) if (z[i] > fmax) fmax = z[i];
   for (i=1; i<=nmax; i++) z[i] /= fmax;

   nn[1] = n  ; col[1] = "blue" ; sty[1] = 1;
   nn[2] = 2*n; col[2] = "green"; sty[2] = 1;
   nn[3] = 3*n; col[3] = "red"  ; sty[3] = 1;
   w.MultiPlot(x,y,y,nn,col,sty,3,10,
               0e0,0e0,0,0e0,0e0,0,0.10,0.45,0.15,0.85,
               "lambda (micron)","u","Spectral energy density");

   w.MultiPlot(x,z,z,nn,col,sty,3,10,
               0e0,0e0,0,0e0,0e0,0,0.60,0.95,0.15,0.85,
               "lambda (micron)","du","Derivative of energy density");

//---------------------------------------------------------------------------
   T = 5778e0;                            // temperature of sun's photosphere
   lam0 = hP*c / (kB*T);                              // reference wavelength
   K = 8e0 * pi * kB * T / pow(lam0,4);                   // factor of u(lam)

   fmin = 1e10; fmax = -1e10;       // positions of maximum and minimum of du
   for (i=1; i<=n; i++) {
      lam = (i-1)*h; f = du(lam);
      if (f > fmax) { fmax = f; lam_1 = lam; }                     // maximum
      if (f < fmin) { fmin = f; lam_2 = lam; }                     // minimum
   }
                                          // find zero of du in [lam_1,lam_2]
   lam_max = 0.5e0 * (lam_1 + lam_2);                // initial approximation
   ierr = NewtonNumDrv(du,lam_1,lam_2,lam_max);
   printf("Wien's law:\n");
   printf("T = %4.0f K\n",T);
   printf("lambda_max = %e m\n",lam_max);
   printf("lambda_max * T = %e m K\n",lam_max * T);        // 2.897768e-3 m K

   w.MainLoop();
}
