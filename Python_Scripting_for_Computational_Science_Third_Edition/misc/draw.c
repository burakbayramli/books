#include <stdlib.h>
#include <math.h>
#include <stdio.h>

/* rquick, inspired by Numerical Recipes */
static unsigned long jquick;
#define IA 8121
#define IC 28411
#define IM 134456
float rquick ()
{
  float r, rr;
  jquick = (jquick*IA + IC) % IM;
  r = (float) jquick / (float) IM;
  rr = abs(r) < 1.0e-20 ? 1.0e-20 : r;   // avoid returning 0.0
  return rr;
}

/* ran3, inspired by Numerical Recipes */

#define MBIG 1000000000
#define MSEED 161803398
#define MZ 0
#define FAC (1.0/MBIG)

float ran3 (long int *idum)
{
  static int inext, inextp;
  static long ma[56];
  static int iff=0;
  long mj, mk;
  int i,ii,k;

  if (*idum < 0 || iff == 0) {
    /* initialization */
    iff=1;
    mj = MSEED - (*idum < 0 ? -*idum : *idum);
    mj %= MBIG;
    ma[55] = mj;
    mk = 1;
    for (i = 1; i <= 54; i++) {
      ii = (21*i) % 55;
      ma[ii] = mk;
      mk = mj - mk;
      if (mk < MZ)
	mk += MBIG;
      mj = ma[ii];
    }
    for (k = 1; k <= 4; k++) {
      for (i = 1; i <= 55; i++) {
	ma[i] -= ma[1+(i+30) % 55];
	if (ma[i] < MZ)
	  ma[i] += MBIG;
      }
    }
    inext = 0;
    inextp = 31;
    *idum = 1;
  }

  /* the generator */
  if (++inext == 56)
    inext = 1;
  if (++inextp == 56)
    inextp = 1;
  mj = ma[inext] - ma[inextp];
  if (mj < MZ)
    mj += MBIG;
  ma[inext] = mj;
  return mj*FAC;
}

/* ncalls and next_value are used in gauss: */
static int ncalls = 0;  
static double next_value;  


/* C_RAND indicates the type of random number generation */
#define PLAIN_LIBC 1
#define QUICK 2
#define RAN3 3

#ifndef C_RAND
#define C_RAND RAN3
#endif

#if C_RAND == PLAIN_LIBC
void setSeed (int seed) { srand (seed); ncalls = 0; }

double draw01 () { return ((double) rand())/((double) RAND_MAX); }
#endif

#if C_RAND == RAN3
void setSeed (int seed) 
{ 
  long int start = (long) -seed; ran3(&start); ncalls = 0; 
}
static long int dummy = 1;

double draw01 () { return (double) ran3(&dummy); }
#endif

#if C_RAND == QUICK
void setSeed (int seed) { jquick = (long) seed; }

double draw01 () { return (double) rquick(); }
#endif


/* draw from the normal distribution */
double gauss (double mean, double stdev)
{
  double tmp1, tmp2, u1, u2;
  ncalls++;
  if ((ncalls % 2) != 0) {
    u1 = draw01();  u2 = draw01();
    tmp1  = sqrt(-2.0*log(u1));
    tmp2  = 2*M_PI*u2;
    next_value = mean + stdev*tmp1*sin(tmp2);
    return       mean + stdev*tmp1*cos(tmp2);
  } else {
    return next_value;
  }
}
