// Normal modes of a loaded string with fixed ends
#include <math.h>
#include "memalloc.h"
#include "eigsys.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double **a, **x, *d, *xp, *yp;
   double h, omega;
   int i, mode, n;
   char title[20];

   n = 100;                                         // number of point masses
   a = Matrix(1,n,1,n);                                 // coefficient matrix
   x = Matrix(1,n,1,n);                       // eigenvectors = displacements
   d = Vector(1,n);                            // eigenvalues ~ frequencies^2
   xp = Vector(1,n);                              // mesh points for plotting
   yp = Vector(1,n);                          // function values for plotting

   for (i=1; i<=n; i++) a[i][i] = 2e0;            // build coefficient matrix
   for (i=2; i<=n-2; i++) { a[i+1][i] = -1e0; a[i][i+1] = -1e0; }

   Jacobi(a,x,d,n);                               // solve eigenvalue problem
   EigSort(x,d,n,1);                     // sort eigenvalues and eigenvectors

   h = 1e0/(n-1);
   for (i=1; i<=n; i++) xp[i] = (i-1) * h;        // mesh points for plotting

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   mode = 1;                                                 // normal mode 1
   omega = sqrt(d[mode])*(n-1);                                  // frequency
   for (i=1; i<=n; i++) yp[i] = x[i][mode];                  // displacements
   sprintf(title,"omega(%i) = %6.2f",mode,omega);
   w.Plot(xp,yp,n,"blue",3,0.10,0.45,0.60,0.90,"x","y",title);

   mode = 2;                                                 // normal mode 2
   omega = sqrt(d[mode])*(n-1);                                  // frequency
   for (i=1; i<=n; i++) yp[i] = x[i][mode];                  // displacements
   sprintf(title,"omega(%i) = %6.2f",mode,omega);
   w.Plot(xp,yp,n,"blue",3,0.60,0.95,0.60,0.90,"x","y",title);

   mode = 3;                                                 // normal mode 3
   omega = sqrt(d[mode])*(n-1);                                  // frequency
   for (i=1; i<=n; i++) yp[i] = x[i][mode];                  // displacements
   sprintf(title,"omega(%i) = %6.2f",mode,omega);
   w.Plot(xp,yp,n,"blue",3,0.10,0.45,0.10,0.40,"x","y",title);

   mode = 4;                                                 // normal mode 4
   omega = sqrt(d[mode])*(n-1);                                  // frequency
   for (i=1; i<=n; i++) yp[i] = x[i][mode];                  // displacements
   sprintf(title,"omega(%i) = %6.2f",mode,omega);
   w.Plot(xp,yp,n,"blue",3,0.60,0.95,0.10,0.40,"x","y",title);

   w.MainLoop();
}
