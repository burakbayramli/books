// Rotation of a molecule to the system of principal axes
#include <math.h>
#include "memalloc.h"
#include "eigsys.h"
#include "coords.h"
#include "matutil.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double *MomInert;
   char title[20];

   MomInert = Vector(1,3);

   int n = 5;                                       // component [0] not used
   double m[] = {0,12.000, 1.000, 1.000, 1.000, 1.000};             // masses
   double x[] = {0, 0.000, 0.635,-0.635,-0.635, 0.635};          // positions
   double y[] = {0, 0.000, 0.635,-0.635, 0.635,-0.635};
   double z[] = {0, 0.000, 0.635, 0.635,-0.635,-0.635};
   double r[] = {0, 0.300, 0.200, 0.200, 0.200, 0.200};              // radii
   char  *col[] = {"", "red", "blue", "blue", "blue", "gray"};      // colors

   double dmax = 1.5e0;          // maximum distance between bonded particles

   m[5] += 1e-10;      // "mark" last particle to set principal symmetry axis

   PyGraph w(argc, argv);
   w.GraphInit(1200,600);

   sprintf(title,"Initial configuration");
   w.PlotParticles(x,y,z,r,col,n,dmax,
                   0e0,0e0,0,0e0,0e0,0,0.05,0.25,0.2,0.8,title);

   MovetoCM(m,x,y,z,n);                                  // move to CM system

   PrincipalAxes(m,x,y,z,n,MomInert,1);      // align main symmetry axis to x
   printf("Structure aligned to x-axis:\n");
   VecPrint(x,n); VecPrint(y,n); VecPrint(z,n);
   printf("Principal moments of inertia:\n");
   VecPrint(MomInert,3);
   sprintf(title,"System of principal axes - main x");
   w.PlotParticles(x,y,z,r,col,n,dmax,
                   0e0,0e0,0,0e0,0e0,0,0.45,0.65,0.2,0.8,title);

   PrincipalAxes(m,x,y,z,n,MomInert,-1);     // align main symmetry axis to z
   printf("\nStructure aligned to z-axis:\n");
   VecPrint(x,n); VecPrint(y,n); VecPrint(z,n);
   printf("Principal moments of inertia:\n");
   VecPrint(MomInert,3);
   sprintf(title,"System of principal axes - main z");
   w.PlotParticles(x,y,z,r,col,n,dmax,
                   0e0,0e0,0,0e0,0e0,0,0.75,0.95,0.2,0.8,title);

   w.MainLoop();
}
