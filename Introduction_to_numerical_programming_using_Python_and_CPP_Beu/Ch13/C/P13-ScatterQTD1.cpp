// Reflexion/transmission of quantum wave packet using tridiagonal solver
#include <complex>
#include "memalloc.h"
#include "pde.h"
#include "graphlib.h"
#define pi 3.141592653589793

using namespace std;
typedef complex<double> dcmplx;                        // define complex type

double Pot(double x, double a, double V0)                        // Potential
   { return fabs(x) <= 0.5e0*a ? V0 : 0e0; }

//===========================================================================
void Init(dcmplx Psi[], double x[], int nx, double x0, double sig, double k0)
//---------------------------------------------------------------------------
// Initial Gaussian wave packet
//    Psi(x,0) = 1/sqrt(sqrt(2*pi)*sig) * exp[-(x-x0)^2/(4*sig^2)] * exp(ikx)
// x0 - position of center, sig - half-width, k0 - average wave number
//---------------------------------------------------------------------------
{
   double a, b, dx, f;
   int i;

   a = 1e0/sqrt(sqrt(2e0*pi)*sig);
   b =-1e0/(4e0*sig*sig);
   for (i=1; i<=nx; i++) {
      dx = x[i] - x0;
      f = a * exp(b*dx*dx); if (f < 1e-10) f = 0e0;
      Psi[i] = f * dcmplx(cos(k0*x[i]),sin(k0*x[i]));
   }
}

//===========================================================================
void ProbDens(dcmplx Psi[], double Psi2[], int nx, double hx, double &PsiNorm)
//---------------------------------------------------------------------------
// Calculates the probability density Psi2[] of the wave function Psi[]
//---------------------------------------------------------------------------
{
   int i;

   for (i=1; i<=nx; i++) {
      Psi2[i] = norm(Psi[i]);             // unnormalized probability density
      if (Psi2[i] < 1e-10) Psi2[i] = 0e0;
   }

   PsiNorm = 0.5e0*(Psi2[1] + Psi2[nx]);      // integral by trapezoidal rule
   for (i=2; i<=(nx-1); i++) PsiNorm += Psi2[i];
   PsiNorm *= hx;

   for (i=1; i<=nx; i++) Psi2[i] /= PsiNorm;      // normalized prob. density
}

int main(int argc, wchar_t** argv)
{
   dcmplx *Psi;
   double *Psi2, *V, *x;
   double a, sig, ht, hx, k0, PsiNorm, t, tmax, V0, x0, xmax;
   int i, it, nout, nt, nx, nx2, nx4;
   char fname[80], title[80];
   FILE *out;

   a    = 5e0;                                  // width of potential barrier
   V0   = 25e0;                                // height of potential barrier
   x0   = -20e0;                           // initial position of wave packet
   sig  = 1e0;                                        // half-width of packet
   k0   = 10e0;                              // average wave number of packet
   xmax = 100e0;                                                 // maximum x
   hx   = 5e-2;                                          // spatial step size
   tmax = 5e0;                                    // maximum propagation time
   ht   = 5e-3;                                                  // time step
   nout = 40;                                      // output every nout steps

   nx = 2*(int)(xmax/hx + 0.5) + 1;            // odd number of spatial nodes
   nt = (int)(tmax/ht + 0.5);                         // number of time steps
   nx2 = nx/2; nx4 = nx/4 + 1;

   Psi  = CVector(1,nx);                                     // wave function
   Psi2 = Vector(1,nx);                                // probability density
   V = Vector(1,nx);                                             // potential
   x = Vector(1,nx);                                    // spatial coordinate

   for (i=1; i<=nx; i++) {             // tabulate spatial mesh and potential
      x[i] = (i-nx2-1)*hx;
      V[i] = Pot(x[i],a,V0);
   }

   Init(Psi,x,nx,x0,sig,k0);                           // initial wave packet

   PyGraph w(argc, argv);
   w.GraphInit(1000,700);

   for (it=1; it<=nt; it++) {                                    // time loop
      t = it*ht;
      PropagQTD(Psi,V,nx,hx,ht);  // propagate solution by tridiagonal solver

      ProbDens(Psi,Psi2,nx,hx,PsiNorm);                // probability density

      if (it % nout == 0 || it == nt) {            // output every nout steps
         sprintf(fname,"scatter_%4.2f.txt",t);
         out = fopen(fname,"w");
         fprintf(out,"t = %4.2f\n",t);
         fprintf(out,"     x          V        PsiR      PsiI      Psi2\n");
         for (i=1; i<=nx; i++)
            fprintf(out,"%10.5f%10.5f%10.5f%10.5f%10.5f\n",
                    x[i],V[i],real(Psi[i]),imag(Psi[i]),Psi2[i]);
         fclose(out);

         w.GraphClear();
         sprintf(title,"Scattering of wave packet  t = %4.2f",t);
         w.Plot(&x[nx4],&Psi2[nx4],nx2,"blue",1,0.15,0.95,0.50,0.90,
                "None","Psi2",title);
         w.Plot(&x[nx4],&V[nx4],nx2,"red",1,0.15,0.95,0.08,0.48,"x","V","");
         w.GraphUpdate();
      }
   }

   w.MainLoop();
}
