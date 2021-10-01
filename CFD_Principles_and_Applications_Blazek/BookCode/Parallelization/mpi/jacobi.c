/******************************************************************************

  Solution of Laplace equation by Jacobi iteration on multiple
  processors using MPI. X-coordinate is associated with the i-
  direction, y-coordinate with j.

  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de

===============================================================================

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpi.h"

int main( int argc, char *argv[] )
{
  char fname[80];
  double *x, *xold, *buffl, *buffr;
  double tol, resid, restot, bci0, bcin, bcj0, bcjn;
  int ntot, n, nitermax, iter, i, ioff, ij, j;
  int rank, nprocs;
  FILE *fp;
  MPI_Status status;

/* start up MPI, get rank and no. of processes */

  MPI_Init( &argc,&argv );
  MPI_Comm_rank( MPI_COMM_WORLD,&rank );
  MPI_Comm_size( MPI_COMM_WORLD,&nprocs );

/* input size of problem; allocate memory for solution vector & buffers.
   Note that there is single layer of dummy points around the domain */

  ntot = 501;                    /* global size; size in j-direction  */
  n    = (ntot-1)/nprocs + 1;    /* size of each block in i-direction */
  ioff = n + 2;                  /* address offset of j-index         */

  nitermax = 10000;              /* max. no. of iterations */
  tol      = 1.e-3;              /* convergence tolerance  */

  bci0 = 5.;                     /* boundary conditions */
  bcin = 5.;
  bcj0 = 5.;
  bcjn = 5.;

  x    = (double *) malloc((size_t)((n+2)*(ntot+2) * sizeof(double)));
  xold = (double *) malloc((size_t)((n+2)*(ntot+2) * sizeof(double)));

  if (rank == 0 && nprocs>1) {
    buffr = (double *) malloc((size_t)((ntot+2) * sizeof(double)));
  } else if (rank>0 && rank<nprocs-1) {
    buffl = (double *) malloc((size_t)((ntot+2) * sizeof(double)));
    buffr = (double *) malloc((size_t)((ntot+2) * sizeof(double)));
  } else if (rank==nprocs-1 && nprocs>1) {
    buffl = (double *) malloc((size_t)((ntot+2) * sizeof(double)));
  }

  printf("proc %i, dim %ix%i\n",rank,n,ntot);

/* initialize solution everywhere */

  for (i=0; i<n+2; i++) {
    for (j=0; j<ntot+2; j++) {
      x[i+ioff*j] = 0.;
    }
  }

/* initialize boundary conditions at j=0, ntot+1 */

  for (i=0; i<n+2; i++) {
    x[i+ioff* 0      ] = bcj0;
    x[i+ioff*(ntot+1)] = bcjn;
  }

/* initialize boundary conditions at i=0, n+1 */

  if (rank == 0) {
    for (j=1; j<ntot+1; j++) {     /* i=0    */
      x[0+ioff*j] = bci0;
    }
  }
  if (rank == nprocs-1) {
    for (j=1; j<ntot+1; j++) {     /* i=n+1 */
      x[n+1+ioff*j] = bcin;
    }
  }

/* iterate the solution ----------------------------------------------------- */

  MPI_Barrier( MPI_COMM_WORLD );   /* synchronize processes */

  iter = 0;
  if (rank == 0) {
    printf("\n");
  }

  do {

    iter++;

/* store previous solution */

    for (ij=0; ij<(n+2)*(ntot+2); ij++) {
      xold[ij] = x[ij];
    }

/* compute new solution */

    for (i=1; i<n+1; i++) {
      for (j=1; j<ntot+1; j++) {
        x[i+ioff*j] = 0.25*(xold[i+1+ioff* j   ]+xold[i-1+ioff* j   ]+
                            xold[i  +ioff*(j+1)]+xold[i  +ioff*(j-1)]);
      }
    }

/* exchange BC's between blocks */

    if (rank==0 && nprocs>1) {
      for (j=1; j<ntot+1; j++) {
        buffr[j] = x[n-1+ioff*j];
      }
      MPI_Send( &buffr[1],ntot,MPI_DOUBLE,rank+1,0,MPI_COMM_WORLD );
      MPI_Recv( &buffr[1],ntot,MPI_DOUBLE,rank+1,1,MPI_COMM_WORLD,&status );
      for (j=1; j<ntot+1; j++) {
        x[n+1+ioff*j] = buffr[j];
      }
    } else if (rank>0 && rank<nprocs-1) {
      for (j=1; j<ntot+1; j++) {
        buffr[j] = x[n-1+ioff*j];
        buffl[j] = x[  2+ioff*j];
      }
      MPI_Send( &buffr[1],ntot,MPI_DOUBLE,rank+1,0,MPI_COMM_WORLD );
      MPI_Send( &buffl[1],ntot,MPI_DOUBLE,rank-1,1,MPI_COMM_WORLD );
      MPI_Recv( &buffr[1],ntot,MPI_DOUBLE,rank+1,1,MPI_COMM_WORLD,&status );
      MPI_Recv( &buffl[1],ntot,MPI_DOUBLE,rank-1,0,MPI_COMM_WORLD,&status );
      for (j=1; j<ntot+1; j++) {
        x[n+1+ioff*j] = buffr[j];
        x[    ioff*j] = buffl[j];
      }
    } else if (rank==nprocs-1 && nprocs>1) {
      for (j=1; j<ntot+1; j++) {
        buffl[j] = x[2+ioff*j];
      }
      MPI_Send( &buffl[1],ntot,MPI_DOUBLE,rank-1,1,MPI_COMM_WORLD );
      MPI_Recv( &buffl[1],ntot,MPI_DOUBLE,rank-1,0,MPI_COMM_WORLD,&status );
      for (j=1; j<ntot+1; j++) {
        x[ioff*j] = buffl[j];
      }
    }

/* compute residual */

    resid = 0.;
    if (rank == 0) {
      for (i=1; i<n+1; i++) {
        for (j=1; j<ntot+1; j++) {
          resid += (x[i+ioff*j]-xold[i+ioff*j])*(x[i+ioff*j]-xold[i+ioff*j]);
        }
      }
    } else {
      for (i=2; i<n+1; i++) {     /* do not add twice at common boundaries */
        for (j=1; j<ntot+1; j++) {
          resid += (x[i+ioff*j]-xold[i+ioff*j])*(x[i+ioff*j]-xold[i+ioff*j]);
        }
      }
    }

/* send/receive residual */

    if (nprocs > 1) {
      MPI_Allreduce( &resid,&restot,1,MPI_DOUBLE,MPI_SUM,MPI_COMM_WORLD );
    } else {
      restot = resid;
    }
    restot = sqrt(restot);

    if (rank == 0) {
      if ((int)fmod((double)iter,50.) == 0) printf("%4i %12.5e\n",iter,restot);
    }

  } while (restot>tol && iter<nitermax);

  if (rank == 0) {
    printf("\nfinal residual after %i iterations: %12.5e\n\n",iter,restot);
  }

/* write out solution */

  sprintf(fname,"plot%i.v2d",rank);

  fp = fopen(fname,"wt");
  fprintf(fp,"Solution of Laplace Equation\n");
  fprintf(fp,"1\n");
  fprintf(fp,"Field Plot\n");
  fprintf(fp,"1 3\n");
  fprintf(fp,"x\ny\nf\n");
  fprintf(fp,"%i %i\n",n,ntot);
  fprintf(fp,"0 0 0\n");
  fprintf(fp,"Region %i\n",rank+1);
  for (j=1; j<ntot+1; j++) {
    for (i=1; i<n+1; i++) {
      fprintf(fp,"%i %i %14.6e\n",i+(n-1)*rank,j,x[i+ioff*j]);
    }
  }
  fclose(fp);

/* finalize */

  MPI_Finalize();
  return(0);
}
