#include <iostream>
#include <stdio.h>
#include <math.h>
#include "polylib.h"

/*
 To compile (in same directory as polylib.c and polylib.h):
g++ -c polylib.c
g++ -c exercise3a.cpp 
g++ -o exercise3a exercise3a.o polylib.o
*/

using namespace std;
using namespace polylib;

double integr(int np, double *w, double *phi1, double *phi2);
double *dvector(int np);
void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M);
void *basis(int np, int P, int i, double *z, double *phi);
double **dmatrix(int Mdim);


main()
{

  int np=10,P=8,Mdim;
  double *z,*w,*p,*phi1,*phi2,**M,sum=0;

  /* enter matrix size */
  Mdim = P+1;

  /* set up vectors and matrices */
  z = dvector(np);
  w = dvector(np);
  p = dvector(np);
  phi1 = dvector(np);
  phi2 = dvector(np);
  M = dmatrix(Mdim);

  /* get zeros and weights */
  zwgll(z, w, np);

  /* assemble element mass matrix */
  assem(Mdim, np, P, z, w, phi1, phi2, M);

  /* generate output */
  cout << "\nM =\n\n"; 
  for(int i=0;i<Mdim;i++){
    for(int j=0;j<Mdim;j++){
      cout << M[i][j] << "\t";
    }
    cout << "\n";
  }
  cout << "\n";
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

double integr(int np, double *w, double *phi1, double *phi2)
{
  register double sum = 0.;

  for(int i=0;i<np;i++){
    sum = sum + phi1[i]*phi2[i]*w[i]; 
  }
  return sum;
}

void *assem(int Mdim, int np, int P, double *z, double *w, double *phi1, double *phi2, double **M)
{
  for(int i=0;i<Mdim;i++){
    basis(np, P, i, z, phi1);                                                        
    for(int j=0;j<Mdim;j++){
      basis(np, P, j, z, phi2);
      M[i][j] = integr(np, phi1, phi2, w);
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

























