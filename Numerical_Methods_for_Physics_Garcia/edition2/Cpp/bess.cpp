#include "NumMeth.h"

void bess( int m_max, double x, Matrix& jj ) {
// Bessel function
// Inputs
//    m_max  Largest desired order
//    x = Value at which Bessel function J(x) is evaluated
// Output
//    jj = Vector of J(x) for order m = 0, 1, ..., m_max

  //* Perform downward recursion from initial guess
  int maxmx = (m_max > x) ? m_max : ((int)x);  // Max(m,x)
  // Recursion is downward from m_top (which is even)
  int m_top = 2*((int)( (maxmx+15)/2 + 1 ));   
  Matrix j(m_top+1);
  j(m_top+1) = 0.0;
  j(m_top) = 1.0;
  double tinyNumber = 1e-16;
  int m;
  for( m=m_top-2; m>=0; m--)       // Downward recursion
    j(m+1) = 2*(m+1)/(x+tinyNumber)*j(m+2) - j(m+3);

  //* Normalize using identity and return requested values
  double norm = j(1);        // NOTE: Be careful, m=0,1,... but
  for( m=2; m<=m_top; m+=2 ) // vector goes j(1),j(2),...
    norm += 2*j(m+1);
  for( m=0; m<=m_max; m++ )  // Send back only the values for
    jj(m+1) = j(m+1)/norm;   // m=0,...,m_max and discard values
}                            // for m=m_max+1,...,m_top
