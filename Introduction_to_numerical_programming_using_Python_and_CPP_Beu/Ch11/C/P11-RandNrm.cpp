// Generation of random numbers with normal distribution
#include <math.h>
#include "random.h"
#include "memalloc.h"
#include "graphlib.h"

double wMet(double x) { return exp(-0.5*x*x); }   // distribution for randMet

int main(int argc, wchar_t** argv)
{
   double a, b, rnd, w, *x, *y;
   long irnd, n, nrnd;

   n = 21;                                        // number of bin boundaries
   nrnd = 1000000;                                // number of random numbers
   x = Vector(1,n); y = Vector(1,n);                       // plotting arrays

   seed();

   PyGraph c(argc, argv);
   c.GraphInit(1200,800);

   a = -3.5e0; b = 3.5e0;
                                                     // Central limit theorem
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   for (irnd=1; irnd<=nrnd; irnd++) HistoBin(randNrm(w),a,b,x,y,n,1);
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.10,0.45,0.60,0.90,"x","n",
          "Mean value of uniform random numbers");
                                                  // 2D Gaussian distribution
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   for (irnd=1; irnd<=nrnd; irnd++) {
      randNrm2(w,rnd,rnd);
      HistoBin(rnd,a,b,x,y,n,1);
   }
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.60,0.95,0.60,0.90,"x","n",
          "2D Gaussian distribution");
                                                         // Metropolis method
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   randMet(wMet,0.1e0,0);                                   // initialize RNG
   for (irnd=1; irnd<=nrnd; irnd++)
      HistoBin(randMet(wMet,0.1e0,1),a,b,x,y,n,1);
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.10,0.45,0.10,0.40,"x","n",
          "Metropolis  delta = 0.1");
                                                         // Metropolis method
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   randMet(wMet,0.5e0,0);                                   // initialize RNG
   for (irnd=1; irnd<=nrnd; irnd++)
      HistoBin(randMet(wMet,0.5e0,1),a,b,x,y,n,1);
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.60,0.95,0.10,0.40,"x","n",
          "Metropolis  delta = 0.5");

   c.MainLoop();
}