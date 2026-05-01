#include "NumMeth.h"

void bess( int m_max, double x, Matrix& jj ) ;

double zeroj( int m_order, int n_zero) {
// Zeros of the Bessel function J(x)
// Inputs
//   m_order   Order of the Bessel function
//   n_zero    Index of the zero (first, second, etc.)
// Output
//   z         The "n_zero"th zero of the Bessel function

  //* Use asymtotic formula for initial guess
  double beta = (n_zero + 0.5*m_order - 0.25)*(3.141592654);
  double mu = 4*m_order*m_order;
  double beta8 = 8*beta;
  double z = beta - (mu-1)/beta8 
             - 4*(mu-1)*(7*mu-31)/(3*beta8*beta8*beta8);

  //* Use Newton's method to locate the root
  Matrix jj(m_order+2); 
  int i;  double deriv;
  for( i=1; i<=5; i++ ) {
    bess( m_order+1, z, jj );  // Remember j(1) is J_0(z)     
    // Use the recursion relation to evaluate derivative
    deriv = -jj(m_order+2) + m_order/z * jj(m_order+1);
    z -= jj(m_order+1)/deriv;  // Newton's root finding  
  }
  return(z);
}
