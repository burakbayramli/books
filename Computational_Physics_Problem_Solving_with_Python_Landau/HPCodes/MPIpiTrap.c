//  From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
// by RH Landau, MJ Paez, and CC Bordeianu
// Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
// Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
// Support by National Science Foundation

//  MPIpi.c computes pi in parallel by Trapezoid Rule Integration
#include "mpi.h"  #include <stdio.h>  #include <math.h>
double f(double);

int main(int argc,char *argv[]) { 
	
  int    n, myid, numprocs, i;
  double PI25DT = 3.141592653589793238462643;
  double mypi, pi, h, sum, x, startwtime=0., endwtime;
  int namelen;
  char   processor_name[MPI_MAX_PROCESSOR_NAME];
  MPI_Init( &argc, &argv);
  MPI_Comm_size( MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank( MPI_COMM_WORLD, &myid);
  MPI_Get_processor_name( processor_name, &namelen);
  fprintf(stdout,"Process %d of %d is on %s\n", myid, numprocs, 
                                                     processor_name);
  fflush( stdout );
  n = 10000;                              // default # of trapezopids
  if (myid == 0) startwtime = MPI_Wtime();
  MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
  h   = 1. / (double) n;
  sum = 0.;
  for (i = myid + 1; i <= n; i += numprocs) {  // Better to work back
    x = h * ((double)i - 0.5);
    sum += f(x); 
  }
  mypi = h * sum;
  MPI_Reduce(&mypi,&pi, 1,MPI_DOUBLE, MPI_SUM,0, MPI_COMM_WORLD);
  if (myid == 0) { 
    endwtime = MPI_Wtime();
    printf("pi is approximately %.16f, Error is %.16f\n",
                                              pi, fabs(pi - PI25DT));
    printf("wall clock time = %f\n", endwtime-startwtime);
    fflush(stdout);    
  }
  MPI_Finalize();
  return 0;
}

double f(double a) { return (4./(1. + a*a));}        // Function f(a)
