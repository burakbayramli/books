// Solves the 2D Poisson equation in a rectangular domain
#include "memalloc.h"
#include "pde.h"
#define pi 3.141592653589793

double Func(double x, double y)                 // RHS function for PoissonXY
   { return cos(x+y) - cos(x-y); }
                                // Coefficients for left and right boundaries
void CondX(double y, double &alf_min, double &bet_min, double &gam_min,
                     double &alf_max, double &bet_max, double &gam_max)
{
   alf_min = 1e0; bet_min = 0e0; gam_min = 0e0;
   alf_max = 1e0; bet_max = 0e0; gam_max = 0e0;
}
                               // Coefficients for lower and upper boundaries
void CondY(double x, double &alf_min, double &bet_min, double &gam_min,
                     double &alf_max, double &bet_max, double &gam_max)
{
   alf_min = 1e0; bet_min = 0e0; gam_min = 0e0;
   alf_max = 1e0; bet_max = 0e0; gam_max = 0e0;
}

int main()
{
   double **u, *x, *y;
   double eps, hx, hy, xmin, xmax, ymin, ymax;
   int i, j, nx, ny;
   FILE *out;

   xmin = -pi; xmax = pi; ymin = -pi; ymax = pi;         // domain boundaries
   nx = 51; ny = 51;                                 // number of mesh points
   eps = 1e-5;                                 // relative solution tolerance

   u = Matrix(1,nx,1,ny);                                         // solution
   x = Vector(1,nx); y = Vector(1,ny);              // mesh point coordinates

   hx = (xmax-xmin)/(nx-1);
   for (i=1; i<=nx; i++) x[i] = xmin + (i-1)*hx;             // x-mesh points
   hy = (ymax-ymin)/(ny-1);
   for (j=1; j<=ny; j++) y[j] = ymin + (j-1)*hy;             // y-mesh points

   for (j=1; j<=ny; j++)             // initial approximation of the solution
      for (i=1; i<=nx; i++) u[i][j] = 0e0;

   PoissonXY(u,x,y,nx,ny,eps,Func,CondX,CondY);

   out = fopen("Poisson.txt","w");
   fprintf(out,"      x         y          u\n");
   for (j=1; j<=ny; j++)
      for (i=1; i<=nx; i++)
         fprintf(out,"%10.5f%10.5f%15.5e\n",x[i],y[j],u[i][j]);
   fclose(out);
}
