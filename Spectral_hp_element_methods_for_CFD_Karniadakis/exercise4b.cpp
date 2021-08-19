#include <iostream>
#include <stdio.h>
#include <math.h>
#include "polylib.h"

/*
 To compile (in same directory as polylib.c and polylib.h):
g++ -c polylib.c
g++ -c exercise4b.cpp 
g++ -o exercise4b exercise4b.o polylib.o -llapack -g2c
*/

using namespace std;
using namespace polylib;

void *assembound(int np, int P, int eln, int Nel, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *Jac, double *u_D, double *bc);
void *rhbc(int np, int P, int eln, int Nel, double *phi, double *z, double *u_D, double *bc);
void *xGlob(int np, int eln,int Nel, double *x, double *x_G);
void *elbound(int Nel, double *bound);
int **imatrix(int nrow, int ncol);
void *mapping(int Nel, int P, int **map);
double integr(int np, double *w, double *phi1, double *phi2);
double *dvector(int np);
int *ivector(int n);
void *func(int np, double *z, double *p);
void *chi(int np, int eln, double *x,double *z, double *Jac, double *bound);
void *assem(int np, int P, int eln, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *Jac);
void *basis(int np, int P, int i, double *z, double *phi);
double **dmatrix(int Mdim);
void *sol(int np, int P, int Nel, double *z, double *phi, double *f_G, double *u_H, double *u_D, double *u_DG, double *bc);
void *GlobAssem(int np, int P, int Nel, int Mdim, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *Jac, double *u_D, double *bc, double **M_G, double *f_G, double *x, double *x_G, double *bound, int **map);

extern "C" {extern void dgetrf_(int *, int *, double (*), int *, int [], int*);}
extern "C" {extern void dgetrs_(unsigned char *, int *, int *, double (*), int *, int [], double [], int *, int *);}

main()
{

  unsigned char TRANS = 'T';
  int np=10,P=8,Mdim,NRHS=1,INFO,*ipiv,Nel=10,**map;
  double *x,*x_G,*z,*w,*p,*f,*f_G,*phi1,*phi2,*u_H,*u_D,*u_DG,*bc,**M,**M_G,sum=0,*Jac,*bound;

  /* enter global matrix size */
  Mdim = Nel*P-1;
  
  /* set up vectors and matrices */
  bc = dvector(2);
  map = imatrix(Nel, P+1);
  Jac = dvector(Nel);
  bound = dvector(Nel+1);
  ipiv = ivector(Mdim);
  u_H = dvector(Nel*(np-1)+1);
  u_D = dvector(np);
  u_DG = dvector(Nel*(np-1)+1);
  x_G = dvector(Nel*(np-1)+1);
  x = dvector(np);
  z = dvector(np);
  w = dvector(np);
  p = dvector(np);
  f = dvector(P+1);
  f_G = dvector(Mdim);
  phi1 = dvector(np);
  phi2 = dvector(np);
  M = dmatrix(P+1);
  M_G = dmatrix(Mdim);

  /* enter boundary conditions (bc[0] is left bc and bc[1] is right bc) */
  bc[0] = 0;
  bc[1] = sin(10.);

  /* get zeros and weights */
  zwgll(z, w, np);

  /* construct grid */
  elbound(Nel, bound);

  /* get mapping array */
  mapping(Nel, P, map);

  /* generate element matrices and assemble to global matrix */
  GlobAssem(np, P, Nel, Mdim, z, w, phi1, phi2, M, f, p, Jac, u_D, bc, M_G, f_G, x, x_G, bound, map);

  /* LU-decomposition using Lapack */
  dgetrf_(&Mdim, &Mdim, M_G[0], &Mdim, ipiv, &INFO);

  /* LU-solve using Lapack */
  dgetrs_(&TRANS, &Mdim, &NRHS, M_G[0], &Mdim, ipiv, f_G, &Mdim, &INFO);

  /* construct global homogeneous and Dirichlet solutions u_H and u_DG */
  sol(np, P, Nel, z, phi1, f_G, u_H, u_D, u_DG, bc);

  /* generate output */
  cout << "\n\nx_G = [";
  for(int i=0;i<Nel*(np-1);i++){
    cout << x_G[i] << ";\n";
  }cout << x_G[Nel*(np-1)] << "];\n";

  cout << "\nu_delta = [";
  for(int i=0;i<Nel*(np-1);i++){
    cout << u_H[i] + u_DG[i] << ";\n";
  }cout << u_H[Nel*(np-1)] + u_DG[Nel*(np-1)] << "];\n\nplot(x_G,u_delta);\n\n";

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
  for(int i=0;i<np;i++){
    p[i] = sin(z[i]);
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

void *assem(int np, int P, int eln, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *Jac)
{
  for(int i=0;i<P+1;i++){
    basis(np, P, i, z, phi1);
    for(int j=0;j<P+1;j++){
      basis(np, P, j, z, phi2);
      M[i][j] = Jac[eln]*integr(np, phi1, phi2, w);
    }
    f[i] = Jac[eln]*integr(np, phi1, p, w);
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

void *sol(int np, int P, int Nel, double *z, double *phi, double *f_G, double *u_H, double *u_D, double *u_DG, double *bc)
{
  for(int i=0;i<((Nel*np)-(Nel-1));i++){
    u_H[i] = 0;
    u_DG[i] = 0;
  }

  for(int eln=0;eln<Nel;eln++){
    if (eln == 0){
      for(int i=1;i<P+1;i++){
	basis(np, P, i, z, phi);
	for(int j=0;j<np;j++){
	  u_H[j + (eln*(np-1))] = u_H[j + (eln*(np-1))] + f_G[i + (eln*P)-1]*phi[j];
	}
      }
    }else if(eln == Nel-1){
      for(int i=0;i<P;i++){
	basis(np, P, i, z, phi);
	for(int j=0;j<np;j++){
	  u_H[j + (eln*(np-1))] = u_H[j + (eln*(np-1))] + f_G[i + (eln*(P))-1]*phi[j];
	}
      }
    }else{
      for(int i=0;i<P+1;i++){
	basis(np, P, i, z, phi);
	for(int j=0;j<np;j++){
	  u_H[j + (eln*(np-1))] = u_H[j + (eln*(np-1))] + f_G[i + (eln*(P))-1]*phi[j];
	}
      }
    }
              if(eln != Nel-1){
      u_H[(np - 1) + eln*(np - 1)] = 0;
     }

  }

  int eln;

  eln = 0;
  rhbc(np, P, eln, Nel, phi, z, u_D, bc);
  for(int i=0;i<np;i++){
    u_DG[i] = u_DG[i] + u_D[i];
  }
  eln = Nel-1;
  rhbc(np, P, eln, Nel, phi, z, u_D, bc);
  for(int i=0;i<np;i++){
    u_DG[i + (Nel*np - (Nel-1))-np] = u_DG[i + (Nel*np - (Nel-1))-np] + u_D[i];
  }

}

void *chi(int np, int eln, double *x, double *z, double *Jac, double *bound)
{
  for(int i=0;i<np;i++){
    x[i] = ((1 - z[i])/2)*bound[eln] + ((1 + z[i])/2)*bound[eln+1];
  }
  Jac[eln] =  (-bound[eln]/2) + (bound[eln+1]/2)  ;
}


void *mapping(int Nel, int P, int **map)
{
  for(int e=0;e<Nel;e++){
    if(e == 0){
      for(int p=0;p<P;p++){
	map[e][p] = p;
      }
    }else if(e == Nel-1){
      for(int p=0;p<P;p++){
	map[e][p] = p + (e*P) - 1;      
      }
    }else{
      for(int p=0;p<P+1;p++){
	map[e][p] = p + (e*P) - 1;
      }
    }
  }
}

int **imatrix(int nrow, int ncol)
{
  register int **A;

  A = (int **)malloc(nrow*sizeof(int *));
  A[0] = (int *)malloc(nrow*ncol*sizeof(int));

  for(int i=1;i<nrow;i++){
    A[i] = A[i-1]+ncol;
  }
  return A;
}

void *elbound(int Nel, double *bound)
{
  for(int i=0;i<Nel+1;i++){
    bound[i] = i;
  }
}

void *xGlob(int np, int eln,int Nel, double *x, double *x_G)
{
  if(eln != Nel-1){
    for(int i=0;i<np-1;i++){
      x_G[i + eln*(np-1)] = x[i];
    }
  }else{
    for(int i=0;i<np;i++){
      x_G[i + eln*(np-1)] = x[i];
    }
  }
}

void *rhbc(int np, int P, int eln, int Nel, double *phi, double *z, double *u_D, double *bc)
{
  int poly, side;

  if(eln == 0){
    poly = 0;
    side = 0;
  }else if(eln == Nel-1){
    poly = P;
    side = 1;
  }

  basis(np, P, poly, z, phi);

  for(int i=0;i<np;i++){
    u_D[i] = bc[side]*phi[i];
  }
}

void *assembound(int np, int P, int eln, int Nel, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *Jac, double *u_D, double *bc)
{
  double sum = 0.0;

  if(eln == 0){
    rhbc(np, P, eln, Nel, phi1, z, u_D, bc);
    for(int i=0;i<P;i++){
      basis(np, P, i+1, z, phi1);
      for(int j=0;j<P;j++){
	basis(np, P, j+1, z, phi2);
	M[i][j] = Jac[eln]*integr(np, phi1, phi2, w);
      }
      f[i] = Jac[eln]*integr(np, phi1, p, w) - Jac[eln]*integr(np, phi1, u_D, w);
    }
  }else if(eln == Nel-1){
    rhbc(np, P, eln, Nel, phi1, z, u_D, bc);
    for(int i=0;i<P;i++){
      basis(np, P, i, z, phi1);
      for(int j=0;j<P;j++){
	basis(np, P, j, z, phi2);
	M[i][j] = Jac[eln]*integr(np, phi1, phi2, w);
      }
      f[i] = Jac[eln]*integr(np, phi1, p, w) - Jac[eln]*integr(np, phi1, u_D, w);
    }
  }
}

void *GlobAssem(int np, int P, int Nel, int Mdim, double *z, double *w, double *phi1, double *phi2, double **M, double *f, double *p, double *Jac, double *u_D, double *bc, double **M_G, double *f_G, double *x, double *x_G, double *bound, int **map)
{
for(int i=0;i<Mdim;i++){
    for(int j=0;j<Mdim;j++){
      M_G[i][j] = 0;
    }
    f_G[i] = 0;
  }

  for(int eln=0;eln<Nel;eln++){
  chi(np, eln, x, z, Jac, bound);
  xGlob(np, eln, Nel, x, x_G);
  func(np, x, p);

  if(eln == 0){
      assembound(np, P, eln, Nel, z, w, phi1, phi2, M, f, p, Jac, u_D, bc);
      for(int a=0;a<P;a++){
	for(int b=0;b<P;b++){
	  M_G[map[eln][a]][map[eln][b]] = M_G[map[eln][a]][map[eln][b]] + M[a][b];
	}
	f_G[map[eln][a]] = f_G[map[eln][a]] + f[a];
      }
    }else if (eln == Nel-1){
      assembound(np, P, eln, Nel, z, w, phi1, phi2, M, f, p, Jac, u_D, bc);
      for(int a=0;a<P;a++){
	for(int b=0;b<P;b++){
	  M_G[map[eln][a]][map[eln][b]] = M_G[map[eln][a]][map[eln][b]] + M[a][b];
	}
	f_G[map[eln][a]] = f_G[map[eln][a]] + f[a];
      }
    }else{
      assem(np, P, eln, z, w, phi1, phi2, M, f, p, Jac);
      for(int a=0;a<P+1;a++){
	for(int b=0;b<P+1;b++){
	  M_G[map[eln][a]][map[eln][b]] = M_G[map[eln][a]][map[eln][b]] + M[a][b];
	}
	f_G[map[eln][a]] = f_G[map[eln][a]] + f[a];
      }
    }
  }
}
