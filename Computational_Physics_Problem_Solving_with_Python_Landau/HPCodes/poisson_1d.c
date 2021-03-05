""" From "COMPUTATIONAL PHYSICS", 3rd Ed, Enlarged Python eTextBook  
    by RH Landau, MJ Paez, and CC Bordeianu
    Copyright Wiley-VCH Verlag GmbH & Co. KGaA, Berlin;  Copyright R Landau,
    Oregon State Unv, MJ Paez, Univ Antioquia, C Bordeianu, Univ Bucharest, 2015.
    Support by National Science Foundation"""

/****************************************************
*    1-D Gauss Siedel solution to Poisson equation  *
*                                                   *
*    Syntax                                         *
*         Poisson  N  N_x                           *
*            ( Max # iteration, grid in x )         *
*                                                   *
*****************************************************/
                                /* Michel Vallieres */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define EPS 1.0e-7

int main ( int argc, char *argv[] )
{
    int    iter, Nx, i, N_iterations;
    double *phi, *source;
    int    *ibound;
    double difference, diff, old, h, chi;

                               /* lattice definition & iteration number */
    if ( argc != 3 )
      {
        printf( " Syntax: poisson  N  N_x \n" );
        exit(1);
      }
    N_iterations = atoi(argv[1]);
    Nx = atoi(argv[2]);
    h = 1.0/(double)(Nx-1);

                               /* make space for arrays */
    phi = (double *)malloc( Nx*sizeof(double) );
    source = (double *)malloc( Nx*sizeof(double) );
    ibound = (int *)malloc( Nx*sizeof(int) );

                               /* initial fields */
                               /* no source      */
    for ( i=0; i<Nx ; i++ )
      {
        ibound[i] = 0;
        phi[i] = 0.0;
        source[i] = 1000 * exp( - 0.1*(i-Nx/2)*(i-Nx/2) );
      }
                               /* boundary conditions */
    ibound[Nx-1] = 1;
    phi[Nx-1] = -1.0;
    ibound[0] = 1;
    phi[0] = 0.0;
                               /* isolated points in grid */
    ibound[Nx/3] = 1;
    phi[Nx/3] = +1.0;

                               /* iterate the solution */
    for ( iter=0; iter<N_iterations ; iter++)
      {
                               /* over-relaxation */
        chi = 1.0;
        if ( iter > 30 ) chi = 1.2;
        if ( iter > 50 ) chi = 1.8;
        diff = 0.0; 
                               /* scan lattice */
        for ( i=0; i<Nx ; i++ ) 
          {
            if ( ibound[i] == 0 )
              { old = phi[i];
                phi[i] = 0.5 * ( phi[i-1] + phi[i+1] - h*h*source[i] );
                phi[i] = (1.0-chi) * old + chi*phi[i];
                if ( fabs( old - phi[i] ) > diff )
                              diff = fabs( old - phi[i] );
              }
          }
                             /* accurate enough ? */ 
        if ( diff < EPS ) break;
      }

    fprintf( stderr,"\n Converged in %d iterations \n", iter );

                             /* check solution */
    diff = 0.0; 
    for ( i=0; i<Nx ; i++ )
      {
        if ( ibound[i] == 0 )
          { 
            difference = 0.5 * ( phi[i-1] - 2*phi[i] + phi[i+1] 
                                   - h*h*source[i] );
            if ( fabs( difference ) > diff )
                           diff = fabs( difference );
          }
      }
    fprintf( stderr," Largest error: %e \n\n", diff );

                             /* output results */
    for ( i=0; i<Nx ; i++ )
       printf( " %f %f \n", i*h, phi[i] );

                             /* free allocated memory */
    free( (char *) phi );
    free( (char *) source);
    free( (char *) ibound);
}
