#include <iostream>
#include <stdio.h>
#include <math.h>
#include "polylib.h"

/*
 To compile (in same directory as polylib.c and polylib.h):
g++ -c polylib.c
g++ -c Helmholtz3.cpp 
g++ -o Helmholtz3 Helmholtz3.o polylib.o -llapack -g2c
*/

using namespace std;
using namespace polylib;

void *diff(int np,int eln, double **D, double *p, double *pd, double *Jac);
double integr(int np, double *w, double *phi1, double *phi2);
void *rhbc(int np, int P, double *phi1, double *phi2, double *z, double *u_D, double *bc, int *Dirichlet);
double *dvector(int np);
int *ivector(int n);
void *chi(int np, double *x, double *z, double *Jac, double *bound);
void *func(int np, double *z, double *p, double lambda);
void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *u_D, double *ud_D, double *Jac, double *dphi1, double *dphi2, double lambda, int eln, double **L, double **D, int *Dirichlet, double *bc);
void *basis(int np, int P, int i, double *z, double *phi);
double **dmatrix(int Mdim);
void *sol(int np, int P, double *z, double *phi1, double *f, double *u_H, int *Dirichlet);

extern "C" {extern void dgetrf_(int *, int *, double (*), int *, int [], int*);}
extern "C" {extern void dgetrs_(unsigned char *, int *, int *, double (*), int *, int [], double [], int *, int *);}

main()
{

  unsigned char TRANS = 'T';
  int np=15,P=8,Mdim,NRHS=1,INFO,*ipiv,*Dirichlet,eln=0;
  double *x,*z,*w,*p,*f,*phi1,*dphi1,*dphi2,*phi2,*u_H,*u_D,*ud_D,*bc,*Jac,*bound,**M,sum=0,lambda=1,**L,**D,**Dt;

  /* enter global matrix size (depends on bc's!)*/
  Mdim = P;

  /* set up vectors and matrices */
  D = dmatrix(np);
  Dt = dmatrix(np);
  Dirichlet = ivector(2);
  ipiv = ivector(Mdim);
  bc = dvector(2);
  bound = dvector(2);
  Jac = dvector(1);  
  x = dvector(np);
  u_D = dvector(np);
  ud_D = dvector(np);
  u_H = dvector(np);
  z = dvector(np);
  w = dvector(np);
  p = dvector(np);
  f = dvector(Mdim);
  phi1 = dvector(np);
  dphi1 = dvector(np);
  phi2 = dvector(np);
  dphi2 = dvector(np);
  M = dmatrix(Mdim);
  L = dmatrix(Mdim);

  /* Dirichlet[0] = 0 means at the left boundary a Neumann bc is posed */
  /* Dirichlet[1] = 1 means at the left boudary a Dirichlet bc is posed */
  /* Dirichlet[1] has similar meanings at the right boundary */
  Dirichlet[0] = 0;
  Dirichlet[1] = 1;
  
  /* enter boundary conditions (bc[0] is left bc and bc[1] is right bc) */
  bc[0] = M_PI;
  bc[1] = 0;
  
  /* construct 'grid' (bound[0] is left boundary and bound[1] is right boundary) */
  bound[0] = -0;
  bound[1] = 1;

  /* get zeros and weights */
  zwgll(z, w, np);

  /* get differentiation matrix D */
  Dgll(D, Dt, z, np);

  /* get mapping details; x-locations and Jacobian */
  chi(np, x, z, Jac, bound);  

  /* calculate p=-(pi^2+lambda)sin(pi*x) at np points x */
  func(np, x, p, lambda);

  /* construct Dirichlet solution u_D */
  rhbc(np, P, phi1, phi2, z, u_D, bc, Dirichlet);

  /* assemble element mass matrix */
  assem(Mdim, np, P, z, w, phi1, phi2, M, f, p, u_D, ud_D, Jac, dphi1, dphi2, lambda, eln, L, D, Dirichlet, bc);

  /* LU-decomposition using Lapack */
  dgetrf_(&Mdim, &Mdim, M[0], &Mdim, ipiv, &INFO);

  /* LU-solve using Lapack */
  dgetrs_(&TRANS, &Mdim, &NRHS, M[0], &Mdim, ipiv, f, &Mdim, &INFO);

  /* construct homogeneous solution u_H */
  sol(np, P, z, phi1, f, u_H, Dirichlet);

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

void *func(int np, double *z, double *p, double lambda)
{   
  for(int i=0;i<np;i++){
    p[i] =-((M_PI*M_PI) + lambda)*sin(M_PI*z[i]);
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

void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *u_D, double *ud_D, double *Jac, double *dphi1, double *dphi2, double lambda, int eln, double **L, double **D, int *Dirichlet, double *bc)
{

  if(Dirichlet[0] == 0 && Dirichlet[1] == 0){
    for(int i=0;i<P+1;i++){
      basis(np, P, i, z, phi1);
      diff(np, eln, D, phi1, dphi1, Jac);
      for(int j=0;j<P+1;j++){
	basis(np, P, j, z, phi2);
	M[i][j] = Jac[eln]*integr(np, w, phi1, phi2);

	diff(np, eln, D, phi2, dphi2, Jac);
	L[i][j] = integr(np, w, dphi1, dphi2)*Jac[eln];

	M[i][j] = -L[i][j] - lambda*M[i][j];
      }
      f[i] = Jac[eln]*(integr(np, w, phi1, p));
      if(i == 0){
	f[i] = f[i] + bc[0];
      }else if(i == P){
	f[i] = f[i] - bc[1];
      }else{
	f[i] = f[i];
      }
    }
  }else if(Dirichlet[0] == 1 && Dirichlet[1] == 0){
    for(int i=0;i<P;i++){
      basis(np, P, i+1, z, phi1);
      diff(np, eln, D, phi1, dphi1, Jac);
      diff(np, eln, D, u_D, ud_D, Jac);
      for(int j=0;j<P;j++){
	basis(np, P, j+1, z, phi2);
	M[i][j] = Jac[eln]*integr(np, w, phi1, phi2);

	diff(np, eln, D, phi2, dphi2, Jac);
	L[i][j] = integr(np, w, dphi1, dphi2)*Jac[eln];

	M[i][j] = -L[i][j] - lambda*M[i][j];
      }
      f[i] = Jac[eln]*(integr(np, w, phi1, p) + integr(np, w, dphi1, ud_D));
      if(i == P-1){
	f[i] = f[i] - bc[1] + Jac[eln]*lambda*integr(np, w, phi1, u_D);
      }else{
	f[i] = f[i] + Jac[eln]*lambda*integr(np, w, phi1, u_D);
      }     
    }
  }else if(Dirichlet[0] == 0 && Dirichlet[1] == 1){
    for(int i=0;i<P;i++){
      basis(np, P, i, z, phi1);
      diff(np, eln, D, phi1, dphi1, Jac);
      diff(np, eln, D, u_D, ud_D, Jac);
      for(int j=0;j<P;j++){
	basis(np, P, j, z, phi2);
	M[i][j] = Jac[eln]*integr(np, w, phi1, phi2);

	diff(np, eln, D, phi2, dphi2, Jac);
	L[i][j] = integr(np, w, dphi1, dphi2)*Jac[eln];

	M[i][j] = -L[i][j] - lambda*M[i][j];
      }
      f[i] = Jac[eln]*(integr(np, w, phi1, p) + integr(np, w, ud_D, dphi1));
      if(i == 0){
	f[i] = f[i] + bc[0] + Jac[eln]*lambda*integr(np, w, phi1, u_D);
      }else{
	f[i] = f[i] + Jac[eln]*lambda*integr(np, w, phi1, u_D);
      }
    }
  }else if(Dirichlet[0] == 1 && Dirichlet[1] == 1){
    for(int i=0;i<P-1;i++){
      basis(np, P, i+1, z, phi1);
      diff(np, eln, D, phi1, dphi1, Jac);
      diff(np, eln, D, u_D, ud_D, Jac);
      for(int j=0;j<P-1;j++){
	basis(np, P, j+1, z, phi2);
	M[i][j] = Jac[eln]*integr(np, w, phi1, phi2);

	diff(np, eln, D, phi2, dphi2, Jac);
	L[i][j] = integr(np, w, dphi1, dphi2)*Jac[eln];

	M[i][j] = -L[i][j] - lambda*M[i][j];
      }
      f[i] = Jac[eln]*(integr(np, w, phi1, p) + lambda*integr(np, w, phi1, u_D) + integr(np, w, dphi1, ud_D));   
    } 
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

void *sol(int np, int P, double *z, double *phi, double *f, double *u_H, int *Dirichlet)
{
  for(int i=0;i<np;i++){
    u_H[i] = 0;
  }

  if(Dirichlet[0] == 0 && Dirichlet[1] == 0){
    for(int i=0;i<P+1;i++){
      basis(np, P, i, z, phi);
      for(int j=0;j<np;j++){
	u_H[j] = u_H[j] + f[i]*phi[j];
      }
    }
  }else if(Dirichlet[0] == 1 && Dirichlet[1] == 0){
    for(int i=0;i<P;i++){
      basis(np, P, i+1, z, phi);
      for(int j=0;j<np;j++){
	u_H[j] = u_H[j] + f[i]*phi[j];
      }
    }
  }else if(Dirichlet[0] == 0 && Dirichlet[1] == 1){
    for(int i=0;i<P;i++){
      basis(np, P, i, z, phi);
      for(int j=0;j<np;j++){
	u_H[j] = u_H[j] + f[i]*phi[j];
      }
    }
  }else if(Dirichlet[0] == 1 && Dirichlet[1] == 1){
    for(int i=0;i<P-1;i++){
      basis(np, P, i+1, z, phi);
      for(int j=0;j<np;j++){
	u_H[j] = u_H[j] + f[i]*phi[j];
      }
    }
  }
}


void *rhbc(int np, int P, double *phi1, double *phi2, double *z, double *u_D, double *bc, int *Dirichlet)
{

  basis(np, P, 0, z, phi1);
  basis(np, P, P, z, phi2);

  for(int i=0;i<np;i++){
    u_D[i] = 0;
  }

  for(int i=0;i<np;i++){
      if(Dirichlet[0] == 1 && Dirichlet[1] == 0){
	u_D[i] = bc[0]*phi1[i];
      }else if(Dirichlet[1] == 1 && Dirichlet[0] == 0){
	u_D[i] = bc[1]*phi2[i];
      }else if(Dirichlet[0] == 1 && Dirichlet[1] == 1){
	u_D[i] = bc[0]*phi1[i] + bc[1]*phi2[i];
      }else{
	u_D[i] = 0;
      }
  }
}



void *chi(int np, double *x, double *z, double *Jac, double *bound)
{
  for(int i=0;i<np;i++){
    x[i] = ((1 - z[i])/2)*bound[0] + ((1 + z[i])/2)*bound[1];
  }
  Jac[0] = (-bound[0]/2) + (bound[1]/2);
}


void *diff(int np, int eln, double **D, double *p, double *pd, double *Jac)
{
  for(int i=0;i<np;i++){
    pd[i] = 0;
    for(int j=0;j<np;j++){
      pd[i] = pd[i] + D[i][j]*p[j]; 
    }
    pd[i] = pd[i]/Jac[eln];
  }
}
