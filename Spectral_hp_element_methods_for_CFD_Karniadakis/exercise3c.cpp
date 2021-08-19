#include <iostream>
#include <stdio.h>
#include <math.h>
#include "polylib.h"

/*
 To compile (in same directory as polylib.c and polylib.h):
g++ -c polylib.c
g++ -c exercise3c.cpp 
g++ -o exercise3c exercise3c.o polylib.o -llapack -g2c
*/

using namespace std;
using namespace polylib;

double integr(int np, double *w, double *phi1, double *phi2);
double *dvector(int np);
int *ivector(int n);
void *func(int np, double *z, double *p);
void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p);
void *basis(int np, int P, int i, double *z, double *phi);
double **dmatrix(int Mdim);
void *sol(int np, int P, double *z, double *phi1, double *f, double *u_delta);

extern "C" {extern void dgetrf_(int *, int *, double (*), int *, int [], int*);}
extern "C" {extern void dgetrs_(unsigned char *, int *, int *, double (*), int *, int [], double [], int *, int *);}

main()
{

  unsigned char TRANS = 'T';
  int np=10,P=8,Mdim,NRHS=1,INFO,*ipiv;
  double *z,*w,*p,*f,*phi1,*phi2,*u_delta,**M,sum=0;

  /* enter matrix size */
  Mdim = P+1;

  /* set up vectors */
  ipiv = ivector(Mdim);
  u_delta = dvector(np);
  z = dvector(np);
  w = dvector(np);
  p = dvector(np);
  f = dvector(Mdim);
  phi1 = dvector(np);
  phi2 = dvector(np);
  M = dmatrix(Mdim);

  /* get zeros and weights */
  zwgll(z, w, np);

  /* calculate p=z^7 at np points z */
  func(np, z, p);

  /* assemble element mass matrix and right-hand-side vector */
  assem(Mdim, np, P, z, w, phi1, phi2, M, f, p);

  /* LU-decomposition using Lapack */
  dgetrf_(&Mdim, &Mdim, M[0], &Mdim, ipiv, &INFO);

  /* LU-solve using Lapack */
  dgetrs_(&TRANS, &Mdim, &NRHS, M[0], &Mdim, ipiv, f, &Mdim, &INFO);

  /* construct solution u_delta */
  sol(np, P, z, phi1, f, u_delta);

  /* generate output */
  cout << "\n\nksi = [";
  for(int i=0;i<np-1;i++){
    cout << z[i] << ";\n";
  }cout << z[np-1] << "];\n";

  cout << "\nu_delta = [";
  for(int i=0;i<np-1;i++){
    cout << u_delta[i] << ";\n";
  }cout << u_delta[np-1] << "];\n\nplot(ksi,u_delta);\n\n";

}

double *dvector(int n)
{
  double *v;

  v = (double *)malloc(n*sizeof(double));
  return v;
}

int *ivector(int n)
{
  int *v;

  v = (int *)malloc(n*sizeof(int));
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
   register int pow = 7;
  
   for(int i = 0;i<np;i++){
    p[i] = 1;
    for(int j = 0; j<pow; j++ ){
      p[i] = p[i]*z[i]; 
    }
  } 
}

double integr(int np, double *w, double *phi1, double *phi2)
{
  register double sum = 0.;

  for(int i=0;i<np;i++){
    sum = sum + phi1[i]*phi2[i]*w[i]; 
  }
  return sum;
}

void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p)
{
  for(int i=0;i<Mdim;i++){
    basis(np, P, i, z, phi1);
    for(int j=0;j<Mdim;j++){
      basis(np, P, j, z, phi2);
      M[i][j] = integr(np, phi1, phi2, w);
    }
    f[i] = integr(np, phi1, p, w);
  }
}


void *basis(int np, int P, int i, double *z, double *phi)
{
  if(i == 0){
    for(int k=0;k<np;k++){
      phi[k] = (1 - z[k])/2;
    }
  }else if(i == P){
    for(int k=0;k<np;k++){
      phi[k] = (1 + z[k] )/2;
    }
  }else{
    jacobfd(np, z, phi, NULL, i-1, 1.0, 1.0);
    for(int k=0;k<np;k++){
      phi[k] = ((1-z[k])/2)*((1+z[k])/2)*phi[k];
    }
  }
}

void *sol(int np, int P, double *z, double *phi, double *f, double *u_delta)
{
  for(int i=0;i<np;i++){
    u_delta[i] = 0;
  }

  for(int i=0;i<P+1;i++){
    basis(np, P, i, z, phi);
    for(int j=0;j<np;j++){
      u_delta[j] = u_delta[j] + f[i]*phi[j];
    }
  }
}























