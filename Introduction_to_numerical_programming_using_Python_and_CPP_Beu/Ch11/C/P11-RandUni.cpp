// Generation of random numbers with uniform distribution
#include "memalloc.h"
#include "random.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double a, b, *x, *y;
   long irnd, n, nrnd;

   n = 21;                                        // number of bin boundaries
   nrnd = 1000000;                                // number of random numbers
   x = Vector(1,n); y = Vector(1,n);                       // plotting arrays

   seed();

   PyGraph c(argc, argv);
   c.GraphInit(1200,800);

   a = 0e0; b = 1e0;
                                                              // Built-in RNG
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   for (irnd=1; irnd<=nrnd; irnd++) HistoBin(random(),a,b,x,y,n,1);
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.10,0.45,0.60,0.90,"x","n",
          "Built-in RNG");
                                           // Linear Congruential Generator 1
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   randLCG1(0);                                             // initialize RNG
   for (irnd=1; irnd<=nrnd; irnd++) HistoBin(randLCG1(1),a,b,x,y,n,1);
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.60,0.95,0.60,0.90,"x","n",
          "Linear Congruential Generator 1");
                                           // Linear Congruential Generator 2
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   randLCG2(0);                                             // initialize RNG
   for (irnd=1; irnd<=nrnd; irnd++) HistoBin(randLCG2(1),a,b,x,y,n,1);
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.10,0.45,0.10,0.40,"x","n",
          "Linear Congruential Generator 2");
                                             // Multiply-with-Carry Generator
   HistoBin(0e0,a,b,x,y,n,0);                         // initialize histogram
   randMCG(0);                                              // initialize RNG
   for (irnd=1; irnd<=nrnd; irnd++) HistoBin(randMCG(1),a,b,x,y,n,1);
   HistoBin(0e0,a,b,x,y,n,2);                          // normalize histogram
   c.Plot(x,y,n,"blue",4,0.60,0.95,0.10,0.40,"x","n",
          "Multiply-with-Carry Generator");

   c.MainLoop();
}