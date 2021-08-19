#include <iostream>
#include <stdio.h>
#include <math.h>
#include "polylib.h"

/*
 To compile (in same directory as polylib.c and polylib.h):
g++ -c polylib.c
g++ -c exercise1a.cpp 
g++ -o exercise1a exercise1a.o polylib.o
*/

using namespace std;
using namespace polylib;

double *dvector(int np);
void *func(int np, double *z, double *p);
double integr(int np, double *w, double *p);


main()
{

  int np=3;
  double *z,*w,*p,sum=0;

  /* set up vectors */
  z = dvector(np);
  w = dvector(np);
  p = dvector(np);

  /* get zeros and weights */
  zwgll(z,w, np);

  /* calculate p=z^6 at np points z */
  func(np, z, p);

  /* integrate p over [-1,1] */
  sum = integr(np, w, p );

  /* generate output */
  cout <<"\nQ = " << np << "\n\n";
  for(int i = 0;i<np;i++ ){
    cout <<"z = " << z[i] << " en p = " << p[i]<<  "\n";
  }
  cout << "\nIntegral = " << sum  << "\n";

}

double *dvector(int n)
{
  double *v;

  v = (double *)malloc(n*sizeof(double));
  return v;
}

void *func(int np, double *z, double *p)
{  
   register int pow = 6;
  
   for(int i = 0;i<np;i++){
    p[i] = 1;
    for(int j = 0; j<pow; j++ ){
      p[i] = p[i]*z[i]; 
    }
  } 
}

double integr(int np, double *w, double *p)
{
  register double sum = 0.;

  for(int i=0;i<np;i++){
    sum = sum + p[i]*w[i]; 
  }
  return sum;
}

