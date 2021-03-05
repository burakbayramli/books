""" From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation"""

/******************************************************************
*    1-D Gauss Siedel solution to Poisson equation                *
*                                                                 *
*    Syntax                                                       *
*         Poisson  N  N_x                                         *
*            ( Max # iteration, grid in x )                       *
*                                                                 *
*    Compilation                                                  *
*    mpicc poisson_parallel_1d.c NR.c -lm -o poisson_parallel_1d  *
*                                                                 *
*    Run                                                          *
*    mpiexec -np 4 ./poisson_parallel_1d 1000 100                 *
*                                                                 *
*                                             Parallel Version    *
*******************************************************************/
                                /* Michel Vallieres */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>


#define EPS 1.0e-7             /* tolerable error     */
#define MAX_PROCS 100          /* max # of processors */
#define MAX_GRID_POINTS 1000   /* max grid points     */


double phi[MAX_GRID_POINTS], source[MAX_GRID_POINTS];
int    ibound[MAX_GRID_POINTS];


/* Gauss-Siedel in local piece of lattice */
void Gauss_Siedel_step( int imin, int imax, int iter, 
                                    double *diff, double h )
{
    double old, chi;
    int i;
                                         /* over-relaxation */
    chi = 1.0;
    if ( iter > 30 ) chi = 1.2;
    if ( iter > 50 ) chi = 1.8;

    *diff = 0.0;
                                         /* scan local lattice */
    for ( i=imin; i<=imax ; i++ )
       {
         if ( ibound[i] == 0 )
           { old = phi[i];
             phi[i] = 0.5 * ( phi[i-1] + phi[i+1] - h*h*source[i] );
             phi[i] = (1.0-chi) * old + chi*phi[i];
             if ( fabs( old - phi[i] ) > *diff )
                            *diff = fabs( old - phi[i] );
           }
       }
}


/* to exchange the ghost rows */
void exchange ( int myid, int numprocs, int imin, int imax )
{
    MPI_Status  recv_status;
    int         top_process, bot_process;

    top_process = myid + 1;
    bot_process = myid - 1;
                                        /* odd - even scheme for safety */
                                        /* & efficiency                 */
    if ( myid % 2 != 0 )
                                        /* odd process */
      {
        if ( myid < numprocs-1 )
           MPI_Ssend( &phi[imax], 1, MPI_DOUBLE, top_process, 121,
                                  MPI_COMM_WORLD);
        if ( myid > 0 )
           MPI_Recv( &phi[imin-1], 1, MPI_DOUBLE, bot_process, 122,
                                  MPI_COMM_WORLD, &recv_status);
        if ( myid > 0 )
           MPI_Ssend( &phi[imin], 1, MPI_DOUBLE, bot_process, 123,
                                  MPI_COMM_WORLD);
        if ( myid < numprocs-1 )
           MPI_Recv( &phi[imax+1], 1, MPI_DOUBLE, top_process, 124,
                                  MPI_COMM_WORLD, &recv_status);
      }
    else
                                        /* even process */
      {
        if ( myid > 0 )
           MPI_Recv( &phi[imin-1], 1, MPI_DOUBLE, bot_process, 121,
                                  MPI_COMM_WORLD, &recv_status);
        if ( myid < numprocs-1 )
           MPI_Ssend( &phi[imax], 1, MPI_DOUBLE, top_process, 122,
                                  MPI_COMM_WORLD);
        if ( myid < numprocs-1 )
           MPI_Recv( &phi[imax+1], 1, MPI_DOUBLE, top_process, 123,
                                  MPI_COMM_WORLD, &recv_status);
        if ( myid > 0 )
           MPI_Ssend( &phi[imin], 1, MPI_DOUBLE, bot_process, 124,
                                  MPI_COMM_WORLD);
      }
}


/* set up local domain - slices */
void decompose_domain ( int myid, int numprocs, int Nx, int *begin_slice,
                       int *end_slice, int *size_slice, int *imin,
                        int *imax )
{
  int ip, j, average, leftover;
  int temp, slice, N_slices;
                                        /* check dimensions */
    if ( numprocs > MAX_PROCS )
      {
        if ( myid == 0 )  fprintf( stderr,
               "Code written for fewer processes than %d \n", MAX_PROCS);
        MPI_Finalize();
        exit(1);
      }
                                        /* partition domain */
    N_slices =  numprocs;
    average = Nx / N_slices;
    leftover = Nx % N_slices;
    temp = -1;
    for ( slice=0 ; slice<N_slices ; slice++ )
    {
      begin_slice[slice] = temp + 1;
      end_slice[slice] = begin_slice[slice] + average - 1;
      if ( leftover >0 )
        {
          end_slice[slice]++;
          leftover--;
        }
      temp = end_slice[slice];
      size_slice[slice] = end_slice[slice] - begin_slice[slice] + 1;
    }
                                        /* echo partitions */
    if( myid == 0 ) 
      {
        fprintf( stderr, "\n Local grid: \n ");
        for ( ip=0 ; ip<numprocs ; ip++ )
           fprintf( stderr, " ( %d , %d )", begin_slice[ip],
                                    end_slice[ip] );
        fprintf( stderr, "\n\n" );
      }
                                        /* local sub-domain */
                                        /* careful with ghost line */
    *imin = begin_slice[myid];
    *imax = end_slice[myid];
}


/* combine sub domains in one -- checks & output */
void gather_check_output ( int myid, int numprocs, int *begin_slice,
                              int *end_slice, int *size_slice,
                                  int Nx, int iter, double h )
{
    int         i, ip;
    double      diff, difference, totaldiff;
    double      *final_phi;
    MPI_Status  recv_status;

    if( myid == 0 )
       fprintf( stderr,"\n Converged in %d iterations \n", iter );

                                        /* exchange the Ghost values */
    exchange ( myid, numprocs, begin_slice[myid], end_slice[myid] );

                                        /* check solution */
    diff = 0.0;
    for ( i=begin_slice[myid] ; i<=end_slice[myid] ; i++ )
      {
        if ( ibound[i] == 0 )
          {
            difference = 0.5 * ( phi[i-1] - 2*phi[i] + phi[i+1]
                                   - h*h*source[i] );
            if ( fabs( difference ) > diff )
                           diff = fabs( difference );
          }
      }
                                        /* find max amongst processes */
    MPI_Allreduce ( &diff, &totaldiff, 1, MPI_DOUBLE,
                                            MPI_MAX, MPI_COMM_WORLD );
    if ( myid == 0 )
        fprintf( stderr," Largest error: %e \n\n", totaldiff );


                                        /* gather pieces of phi */
    if ( myid == 0 )
      {
                                        /* space to gather final phi */
         final_phi = (double *)malloc( Nx*sizeof(double) );

                                        /* transfer own sub-domain */
                                        /* piece of phi            */
         for ( i=begin_slice[myid]; i<=end_slice[myid]; i++ )
           {
             final_phi[i] = phi[i];
           }
                                        /* receives pieces of    */
                                        /* phi from sub-domains  */
         for ( ip=1 ; ip<numprocs ; ip++ )
           {
              MPI_Recv( &final_phi[begin_slice[ip]], 
                      size_slice[ip], MPI_DOUBLE, ip, 229,
                                  MPI_COMM_WORLD, &recv_status); 
           }
                                        /* output results */
         for ( i=0; i<Nx ; i++ )
                   printf( " %f %f \n", i*h, final_phi[i] );

	                                /* free up memory space */
         free( (char *) final_phi );
      }
                                        /* nodes send phi to node 0 */
    else
      {
         MPI_Ssend( &phi[begin_slice[myid]], size_slice[myid], 
                       MPI_DOUBLE, 0, 229, MPI_COMM_WORLD );
      }

}



int main ( int argc, char *argv[] )
{
    int    iter, Nx, i, is, N_iterations;
    double difference, diff, totaldiff, h, chi;
    int    myid, numprocs;
    int    begin_slice[MAX_PROCS], end_slice[MAX_PROCS],
           size_slice[MAX_PROCS], imin, imax;

                                        /* join the MPI virtual machine */
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);

                                        /* lattice definition & iteration number */
    if ( argc != 3 )
      {
        if ( myid == 0 )
            printf( " Syntax: poisson  N  N_x \n" );
        MPI_Finalize();
        exit(1);
      }
    N_iterations = atoi(argv[1]);
    Nx = atoi(argv[2]);
    h = 1.0/(double)(Nx-1);

                                        /* domain decomposition */
    decompose_domain ( myid, numprocs, Nx, begin_slice,
                         end_slice, size_slice, &imin, &imax );

                                        /* initial fields */
    for ( i=imin; i<=imax ; i++ )
      {
        ibound[i] = 0;
        phi[i] = 0.0;
        source[i] = 1000 * exp( - 0.1*(i-Nx/2)*(i-Nx/2) );
      }
                                        /* boundary conditions */
    if ( myid == numprocs - 1 )
      {
         ibound[imax] = 1;
         phi[imax] = -1.0;
      }
    if ( myid == 0 )
      {
        ibound[imin] = 1;
        phi[imin] = 0.0;
      }
                                        /* isolated points in grid */
    is = Nx/3;
    if ( is >= imin || is <= imax )
      { 
         ibound[is] = 1;
         phi[is] = +1.0;
      }

                                        /* iterate the solution */
    for ( iter=0; iter<N_iterations ; iter++)
      {

                                        /* exchange the Ghost values */
        exchange ( myid, numprocs, imin, imax );

                                       /* one Gauss-Siedel iteration */
        Gauss_Siedel_step( imin, imax, iter, &diff, h );

                                       /* maximum of error from */
                                       /* each sub-domain       */
        MPI_Allreduce ( &diff, &totaldiff, 1, MPI_DOUBLE,
                                            MPI_MAX, MPI_COMM_WORLD );

                                       /* accurate enough ? */ 
        if ( totaldiff < EPS ) break;
      }

                                       /* gather, check, & output results */
    gather_check_output ( myid, numprocs, begin_slice,
                       end_slice, size_slice,  Nx, iter, h );

                                       /* we are done */
    MPI_Finalize();
    exit(1);
}

