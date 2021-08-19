#include <iostream>
#include <stdio.h>
#include <math.h>
#include "polylib.h"

/*
 To compile (in same directory as polylib.c and polylib.h):
g++ -c polylib.c
g++ -c exercise3e.cpp 
g++ -o exercise3e exercise3e.o polylib.o -llapack -g2c
*/

using namespace std;
using namespace polylib;

double integr(int np, double *w, double *phi1, double *phi2);
void *rhbc(int np, int P, double *phi, double *z, double *u_D, double *bc);
double *dvector(int np);
int *ivector(int n);
void *chi(int np, double *x, double *z, double *Jac, double *bound);
void *func(int np, double *z, double *p);
void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *assem, double *Jac);
void *basis(int np, int P, int i, double *z, double *phi);
double **dmatrix(int Mdim);
void *sol(int np, int P, double *z, double *phi1, double *f, double *u_H);

extern "C" {extern void dgetrf_(int *, int *, double (*), int *, int [], int*);}
extern "C" {extern void dgetrs_(unsigned char *, int *, int *, double (*), int *, int [], double [], int *, int *);}

main()
{

  unsigned char TRANS = 'T';
  int np=10,P=8,Mdim,NRHS=1,INFO,*ipiv;
  double *x,*z,*w,*p,*f,*phi1,*phi2,*u_H,*u_D,*bc,*Jac,*bound,**M,sum=0;

  /* enter matrix size */
  Mdim = P-1;

  /* set up vectors and matrices */
  ipiv = ivector(Mdim);
  bc = dvector(2);
  bound = dvector(2);
  Jac = dvector(1);  
  x = dvector(np);
  u_D = dvector(np);
  u_H = dvector(np);
  z = dvector(np);
  w = dvector(np);
  p = dvector(np);
  f = dvector(Mdim);
  phi1 = dvector(np);
  phi2 = dvector(np);
  M = dmatrix(Mdim);

  /* enter boundary conditions (bc[0] is left bc and bc[1] is right bc) */
  bc[0] = 128;
  bc[1] = 78125;

  /* enter boundaries (bound[0] is left boundary and bound[1] is right
     boundary) */
  bound[0] = 2;
  bound[1] = 5;

  /* get zeros and weights */
  zwgll(z, w, np);

  /* get mapping details; x-locations and Jacobian */
  chi(np, x, z, Jac, bound);

  /* calculate p=x^7 at np points x*/   
  func(np, x, p);

  /* get known solution u_D which satisfies boundary conditions */
  rhbc(np, P, phi1, z, u_D, bc);

  /* assemble element mass matrix */
  assem(Mdim, np, P, z, w, phi1, phi2, M, f, p, u_D, Jac);

  /* LU-factorise using Lapack */
  dgetrf_(&Mdim, &Mdim, M[0], &Mdim, ipiv, &INFO);

  /* LU-solve using Lapack */
  dgetrs_(&TRANS, &Mdim, &NRHS, M[0], &Mdim, ipiv, f, &Mdim, &INFO);

  /* construct solution u_delta */
  sol(np, P, z, phi1, f, u_H);

  /* generate output */
  cout << "\n\nx = [";
  for(int i=0;i<np-1;i++){
    cout << x[i] << ";\n";
  }cout << x[np-1] << "];\n";

  cout << "\nu_delta = [";
  for(int i=0;i<np-1;i++){
    cout << u_H[i] + u_D[i] << ";\n";
  }cout << u_H[np-1] + u_D[np-1] << "];\n\nplot(x,u_delta);\n\n";
 
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

void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *u_D, double *Jac)
{
  for(int i=0;i<Mdim;i++){
    basis(np, P, i+1, z, phi1);
    for(int j=0;j<Mdim;j++){
      basis(np, P, j+1, z, phi2);
      M[i][j] = Jac[0]*integr(np, phi1, phi2, w);
    }
    f[i] = Jac[0]*integr(np, phi1, p, w) - Jac[0]*integr(np, phi1, u_D, w);
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

void *sol(int np, int P, double *z, double *phi, double *f, double *u_H)
{
  for(int i=0;i<np;i++){
    u_H[i] = 0;
  }

  for(int i=0;i<P-1;i++){
    basis(np, P, i+1, z, phi);
    for(int j=0;j<np;j++){
      u_H[j] = u_H[j] + f[i]*phi[j];
    }
  }
}

void *rhbc(int np, int P, double *phi, double *z, double *u_D, double *bc)
{
  basis(np, P, 0, z, u_D);
  basis(np, P, P, z, phi);
  for(int i=0;i<np;i++){
    u_D[i] = bc[0]*u_D[i] + bc[1]*phi[i];
  }
}


void *chi(int np, double *x, double *z, double *Jac, double *bound)
{
  for(int i=0;i<np;i++){
    x[i] = ((1 - z[i])/2)*bound[0] + ((1 + z[i])/2)*bound[1];
  }
  Jac[0] = (-bound[0]/2) + (bound[1]/2);
}




















