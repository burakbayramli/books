// Steady-state temperature distribution in a thin conducting disk
#include "memalloc.h"
#include "pde.h"
#include "graphlib.h"

double Func(double x, double y)                 // RHS function for Poisson2D
   { return 0e0; }

//===========================================================================
void BoundCondEllipse(double **u, int imin[], int imax[], int nx, int ny)
//---------------------------------------------------------------------------
// Determines the boundary indexes imin[j] and imax[j] (j=1,ny) for an
// elliptical domain, determined by the number of mesh points nx and ny, and
// sets values on the boundaries for the function u
//---------------------------------------------------------------------------
{
   double a, b, x0, xi, y0, yj;
   int i, j;

   x0 = 0.5e0*(nx + 1e0);                  // center of dimensionless ellipse
   y0 = 0.5e0*(ny + 1e0);
   a = x0 - 1e0;                                                 // semi-axes
   b = y0 - 1e0;

   for (j=1; j<=ny; j++) {                                // boundary indexes
      yj = j - y0;                                   // relative y-coordinate
      xi = a * sqrt(1e0 - yj*yj/(b*b));            // x-coordinate on ellipse
      if (xi == 0e0) xi = 0.75e0;          // correction for 1-point boundary

      imin[j] = int(x0 - xi + 0.5e0);                  // left boundary index
      imax[j] = int(x0 + xi + 0.5e0);                 // right boundary index
   }
                                                           // boundary values
   for (i=imin[1]; i<=imax[1]; i++) u[i][1] = 0e0;                  // bottom
   for (j=2; j<=ny; j++) {
      u[imin[j]][j] = 100e0 * (nx - 2*imin[j])/(nx-1);                // left
      u[imax[j]][j] = 0e0;                                           // right
   }
   for (i=imin[ny]; i<=imax[ny]; i++) u[i][ny] = 0e0;                  // top
}

int main(int argc, wchar_t** argv)
{
   double **u, *x, *y;
   int *imin, *imax;
   double a, eps, hx, hy, umin, umax, xmin, xmax, ymin, ymax;
   int i, j, nx, ny;
   FILE *out;

   a = 5;                                                      // disk radius
   xmin = -a; xmax = a; ymin = -a; ymax = a;             // domain boundaries
   nx = 51; ny = 51;                                 // number of mesh points
   eps = 1e-5;                                 // relative solution tolerance

   u = Matrix(1,nx,1,ny);                                         // solution
   x = Vector(1,nx); y = Vector(1,ny);              // mesh point coordinates
   imin = IVector(1,ny);                      // indexes of y-line boundaries
   imax = IVector(1,ny);

   hx = (xmax-xmin)/(nx-1);
   for (i=1; i<=nx; i++) x[i] = xmin + (i-1)*hx;             // x-mesh points
   hy = (ymax-ymin)/(ny-1);
   for (j=1; j<=ny; j++) y[j] = ymin + (j-1)*hy;             // y-mesh points

   for (j=1; j<=ny; j++)                               // initialize solution
      for (i=1; i<=nx; i++) u[i][j] = -1e99;

   BoundCondEllipse(u,imin,imax,nx,ny);                    // boundary values

   for (j=2; j<=ny-1; j++)           // initial approximation of the solution
      for (i=imin[j]+1; i<=imax[j]-1; i++) u[i][j] = 0e0;   // interior nodes

   Poisson2D(u,x,y,imin,imax,nx,ny,eps,Func);

   out = fopen("Poisson.txt","w");
   fprintf(out,"      x         y          u\n");
   for (j=1; j<=ny; j++)
      for (i=1; i<=nx; i++)
         fprintf(out,"%10.5f%10.5f%15.5e\n",x[i],y[j],u[i][j]);
   fclose(out);

   umin = umax = u[imin[1]][1];        // minimum and maximum of the solution
   for (j=1; j<=ny; j++)
      for (i=imin[j]; i<=imax[j]; i++) {
         if (u[i][j] < umin) umin = u[i][j];
         if (u[i][j] > umax) umax = u[i][j]; 
      }

   PyGraph w(argc, argv);
   w.GraphInit(800,800);
   w.Contour(u,nx,ny,xmin,xmax,ymin,ymax,umin,umax,
             0.15,0.85,0.15,0.85,"x","y","Disk temperature");
   w.MainLoop();
}
