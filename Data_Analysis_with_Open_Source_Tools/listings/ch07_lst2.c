
#include <stdio.h>
#include <gsl/gsl_min.h>

double fct( double x, void *params ) { 
  return x*x*log(x);
}

int main() {
  double a = 0.1, b = 1; /* interval which bounds the minimum */

  gsl_function f;        /* pointer to the function to minimize */
  gsl_min_fminimizer *s; /* pointer to the minimizer instance */

  f.function = &fct;     /* the function to minimize */
  f.params = NULL;       /* no additional parameters needed */

  /* allocate the minimizer, choosing a particular algorithm */
  s = gsl_min_fminimizer_alloc( gsl_min_fminimizer_goldensection );

  /* initialize the minimizer with a function an an initial interval */
  gsl_min_fminimizer_set( s, &f, (a+b)/2.0, a, b );

  while ( b-a > 1.e-6 ) {
    /* perform one minimization step */
    gsl_min_fminimizer_iterate( s );

    /* obtain the new bounding interval */
    a = gsl_min_fminimizer_x_lower( s );
    b = gsl_min_fminimizer_x_upper( s );

    printf( "%f\t%f\n", a, b );
  } 

  printf( "Minimum Position: %f\tValue: %f\n", 
	  gsl_min_fminimizer_x_minimum(s), gsl_min_fminimizer_f_minimum(s) );

  gsl_min_fminimizer_free( s );

  return 0;
}
