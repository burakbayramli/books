//-------------------------------- random.h ---------------------------------
// Contains routines for generating pseudo-random numbers.
// Part of the numxlib numerics library. Author: Titus Beu, 2013
//---------------------------------------------------------------------------
#ifndef _RANDOM_
#define _RANDOM_

#include <stdlib.h>
#include <time.h>
#include <math.h>

// Initializes the random number generator using the current system time
#define seed() srand((unsigned)time(NULL))

// Generates random floating point numbers in the range [0,1)
#define random() ((double)rand()/(RAND_MAX+1))

// Generates random numbers with exponential distribution
#define randExp() -log(fabs(1e0 - random()))

//===========================================================================
double randLCG1(int iopt)
//---------------------------------------------------------------------------
// Linear Congruential Generator for real random numbers in the range [0,1)
// Press, Teukolsky, Vetterling, Flannery, Numerical Recipes 3rd Ed., 2007
// iopt = 0 initializes the sequence
//---------------------------------------------------------------------------
{
   const unsigned int a = 8121, c = 28411, m = 134456;
   static unsigned int irnd;                       // conserved between calls

   if (iopt == 0) irnd = rand();                            // initialization

   irnd = (a * irnd + c) % m;
   return (double)irnd/m;
}

//===========================================================================
double randLCG2(int iopt)
//---------------------------------------------------------------------------
// Linear Congruential Generator for real random numbers in the range [0,1)
// D. Rapaport, The Art of Molecular Dynamics Simulation, Cambridge, 2004
// iopt = 0 initializes the sequence
//---------------------------------------------------------------------------
{
   const unsigned int a = 314159269, c = 453806245, m = 2147483647;
   static unsigned int irnd;                       // conserved between calls

   if (iopt == 0) irnd = rand();                            // initialization

   irnd = (a * irnd + c) & m;
   return (double)irnd/m;
}

//===========================================================================
double randMCG(int iopt)
//---------------------------------------------------------------------------
// Multiply-with-Carry Generator for real random numbers in the range [0,1)
// George Marsaglia, post to Sci. Stat. Math, 1999
// iopt = 0 initializes the sequence
//---------------------------------------------------------------------------
{
   static unsigned int irand1, irand2;             // conserved between calls

   if (iopt == 0) { irand1 = rand(); irand2 = rand(); }     // initialization

   irand1 = 36969 * (irand1 & 0xFFFF) + (irand1 >> 16);
   irand2 = 18000 * (irand2 & 0xFFFF) + (irand2 >> 16);
   return (double)((irand1 << 16) + irand2)/0xFFFFFFFF;
}

//===========================================================================
double randNrm(double &w)
//---------------------------------------------------------------------------
// Returns random numbers with normal distribution 
// w = exp(-0.5e0*x*x) / sqrt(2*pi)
// using the central limit theorem and sums of 12 uniform random numbers
//---------------------------------------------------------------------------
{
   double sum, x;
   int i;

   sum = 0e0;
   for (i=1; i<=12; i++) sum += random();
   x = sum - 6e0;
   w = 0.398942280401433 * exp(-0.5e0*x*x);       // 1/sqrt(2*pi) = 0.3989...
   return x;
}

//===========================================================================
void randNrm2(double &w, double &x, double &y)
//---------------------------------------------------------------------------
// Returns 2 random numbers (x,y) with normal distribution
// w = exp(-(x*x+y*y)/2) / (2*pi)
// and the corresponding distribution value 
//---------------------------------------------------------------------------
{
#define pi2 6.283185307179586
   double r, r2, theta;

   r2 = -log(1e0 - random());          // exponential distribution for r**2/2
   w = exp(-r2) / pi2;                         // distribution function value
   r = sqrt(2e0*r2); theta = pi2 * random();             // polar coordinates
   x = r * cos(theta); y = r * sin(theta);           // Cartesian projections
}

//===========================================================================
double randMet(double w(double), double delta, int iopt)
//---------------------------------------------------------------------------
// Generates random numbers x with the distribution w(x) by the Metropolis
// method, using a maximal trial step size delta
// iopt = 0 initializes the sequence
//---------------------------------------------------------------------------
{
   static double dx, xrnd;                         // conserved between calls

   if (iopt == 0) xrnd = random();                          // initialization

   dx = delta * (2e0*random() - 1e0);                      // trial step size
   if (random() <= w(xrnd+dx)/w(xrnd)) xrnd += dx;  // accept with prob. w(x)

   return xrnd;
}

#endif
