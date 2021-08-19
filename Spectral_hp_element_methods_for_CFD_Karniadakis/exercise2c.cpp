#include <iostream>
#include <stdio.h>
#include <math.h>
#include "polylib.h"

/*
To compile (in same directory as polylib.c and polylib.h):
g++ -c -g polylib.c
g++ -c -g exercise2c.cpp
g++ -o exercise2c exercise2c.o polylib.o
*/

using namespace std;
using namespace polylib;

double integr(int np, double *w, double *p);
double *dvector(int np);
double **dmatrix(int n);
void *func(int np, double *z, double *p);
void *diff(int np, double **D, double *p, double *pd, double *Jac);
void *chi(int np, double *x, double *z, double *Jac, double *bound);

main(){
{
  int np=5;
  double *w, *x,*z,*p,*pd,**D,**Dt,*Jac,*bound,sum=0;

  /* set up vectors and matrices */
  x = dvector(np);
  z = dvector(np);
  w = dvector(np);
  p = dvector(np);
  pd = dvector(np);
  D = dmatrix(np);
  Dt = dmatrix(np);
  Jac = dvector(1);
  bound = dvector(2);

  /* define domain [bound[0],bound[1]] */
  bound[0] = 0;
  bound[1] = M_PI/2;

  /* get zeros and weights */
  zwgll(z, w, np);

  /* get differentiation matrix D */
  Dgll(D, Dt, z, np);

  /* get mapping details; x-locations and Jacobian */
  chi(np, x, z, Jac, bound);

  /* calculate p=-cos(x) at np points x */
  func(np, x, p);
 
  /* differentiate p and put result in pd */
  diff(np, D, p, pd, Jac);
  
  /* integrate pd over [bound[0],bound[1]] */
  sum = Jac[0]*integr(np, w, pd);
  
  /* generate output */
  cout << "\n  x\n\n";
  for(int i=0;i<np;i++){
    cout << x[i] << "\n";
  }cout << "\n\n du/dx\n\n";
  for(int i=0;i<np;i++){
    cout << pd[i] << "\n";
  }cout << "\n";
  cout << "Integral = " << sum << "\n\n";
  cout << "error = " << sum - 1 << "\n\n";
  
}

double *dvector(int n)
{
  double *v;

  v = (double *)malloc(n*sizeof(double));
 return v;
}

double **dmatrix(int n)
{
  double **A;
  A = (double **)malloc(n*sizeof(double *));
  A[0] = (double *)malloc(n*n*sizeof(double));

  for(int i=1;i<n;i++){
    A[i] = A[i-1]+n;
  }
  return A;
}

void *func(int np, double *z, double *p)
{
  for(int i=0;i<np;i++){
    p[i] = -cos(z[i]);
  }
}

void *diff(int np, double **D, double*p, double *pd, double *Jac)
{
  for(int i=0;i<np;i++){
    pd[i] = 0;
    for(int j=0;j<np;j++){
      pd[i] = pd[i] + D[i][j]*p[j]; 
    }
    pd[i] = pd[i]/Jac[0];
  }
}

void *chi(int np, double *x, double *z, double *Jac,double *bound)
{
  for(int i=0;i<np;i++){
    x[i] = ((1 - z[i])/2)*bound[0] + ((1 + z[i])/2)*bound[1];
  }
  Jac[0] = (-bound[0]/2) + (bound[1]/2);
}

double integr(int np, double *w, double *p)
{
  register double sum = 0.;

  for(int i=0;i<np;i++){
    sum = sum + p[i]*w[i]; 
  }
  return sum;
}
