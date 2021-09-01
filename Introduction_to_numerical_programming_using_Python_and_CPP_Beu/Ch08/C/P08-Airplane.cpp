// Rotation of an airplane
#include <math.h>
#include "memalloc.h"
#include "eigsys.h"
#include "coords.h"
#include "matutil.h"
#include "graphlib.h"

#define pi 3.141592653589793

int main(int argc, wchar_t** argv)
{
   double *MomInert;
   double cosp, f, phi, sinp, xi, yi;
   int i;
   char title[20];

   MomInert = Vector(1,3);

   int n = 10;                                      // component [0] not used
   double m[] = {0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0};                  // masses
   double x[] = {0,-2,-6, 2, 0, 2, 6, 1, 2,-2, 2};               // positions
   double y[] = {0,-2, 2,-6, 2, 0, 6, 2, 1,-2, 2};
   double z[] = {0, 0, 0, 0, 0, 0, 0, 1, 1, 4, 1};

   int n3 = 9;                  // indexes of triangles defining the fuselage
   int ind1[] = {0, 1, 1, 1, 1, 1, 1, 1, 6, 1};
   int ind2[] = {0, 2, 3, 4, 5, 4, 5, 7, 7, 9};
   int ind3[] = {0, 4, 5, 6, 6, 7, 8, 8, 8,10};

   PyGraph w(argc, argv);
   w.GraphInit(800,800);

   sprintf(title,"Initial configuration");
   w.PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
                0e0,0e0,0,0e0,0e0,0,0.05,0.45,0.55,0.92,title);

   MovetoCM(m,x,y,z,n);                                  // move to CM system

   PrincipalAxes(m,x,y,z,n,MomInert,1);  // rotate airplane to principal axes
   printf("Airplane in system of principal axes:\n");
   VecPrint(x,6); VecPrint(y,6); VecPrint(z,6);
   sprintf(title,"System of principal axes");
   w.PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
                0e0,0e0,0,0e0,0e0,0,0.55,0.95,0.55,0.92,title);

   f = pi/180e0;

   phi = 20e0;                                                         // yaw
   cosp = cos(phi*f); sinp = sin(phi*f);
   for (i=1; i<=n; i++) {                     // rotate airplane about z-axis
      xi = x[i];
      x[i] = cosp * xi - sinp * y[i];
      y[i] = sinp * xi + cosp * y[i];
   }
   printf("\nRotated airplane yaw = %4.1f\n",phi);
   VecPrint(x,6); VecPrint(y,6); VecPrint(z,6);
   sprintf(title,"yaw = %4.1f",phi);
   w.PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
                0e0,0e0,0,0e0,0e0,0,0.05,0.45,0.05,0.42,title);

   phi = -35e0;                                                       // roll
   cosp = cos(phi*f); sinp = sin(phi*f);
   for (i=1; i<=n; i++) {                     // rotate airplane about x-axis
      yi = y[i];
      y[i] = cosp * yi - sinp * z[i];
      z[i] = sinp * yi + cosp * z[i];
   }
   printf("\nRotated airplane roll = %4.1f\n",phi);
   VecPrint(x,6); VecPrint(y,6); VecPrint(z,6);
   sprintf(title,"roll = %4.1f",phi);
   w.PlotStruct(x,y,z,n,ind1,ind2,ind3,n3,
                0e0,0e0,0,0e0,0e0,0,0.55,0.95,0.05,0.42,title);

   w.MainLoop();
}
