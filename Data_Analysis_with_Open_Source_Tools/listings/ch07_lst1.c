
#include <stdio.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

int main() {
  double r;

  gsl_vector *a, *b, *s, *t;
  gsl_matrix *m, *v;


  /* --- Vectors --- */
  a = gsl_vector_alloc( 2 );
  b = gsl_vector_alloc( 2 );

  /* a = [ 1.0, 2.0 ] */
  gsl_vector_set( a, 0, 1.0 ); 
  gsl_vector_set( a, 1, 2.0 );

  /* a = [ 3.0, 6.0 ] */
  gsl_vector_set( b, 0, 3.0 );
  gsl_vector_set( b, 1, 6.0 );
 
  /* a += b (so that now a = [ 4.0, 8.0 ]) */
  gsl_vector_add( a, b );
  gsl_vector_fprintf( stdout, a, "%f" );

  /* a . b (dot product) */
  gsl_blas_ddot( a, b, &r );
  fprintf( stdout, "%f\n", r );


  /* --- Matrices --- */
  s = gsl_vector_alloc( 2 );
  t = gsl_vector_alloc( 2 ); 

  m = gsl_matrix_alloc( 2, 2 );
  v = gsl_matrix_alloc( 2, 2 );

  /* m = [ [1, 2], 
           [0, 3] ] */
  gsl_matrix_set( m, 0, 0, 1.0 );
  gsl_matrix_set( m, 0, 1, 2.0 );
  gsl_matrix_set( m, 1, 0, 0.0 );
  gsl_matrix_set( m, 1, 1, 3.0 );

  /* m = U s V^T (SVD : singular values are in vector s) */
  gsl_linalg_SV_decomp( m, v, s, t );
  gsl_vector_fprintf( stdout, s, "%f" );


  /* --- Cleanup --- */
  gsl_vector_free( a );
  gsl_vector_free( b );
  gsl_vector_free( s );
  gsl_vector_free( t );

  gsl_matrix_free( m );
  gsl_matrix_free( v );

  return 0;
}
