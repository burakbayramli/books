#include "NumMeth.h"

double rand( long& seed );

// Random number generator; Normal (Gaussian) dist.
double randn( long& seed ) {
// Input
//   seed    Integer seed  (DO NOT USE A SEED OF ZERO)
// Output
//	 randn   Random number, Gaussian distributed

  double randn = sqrt( -2.0*log(1.0 - rand(seed)) )
	        * cos( 6.283185307 * rand(seed) );
  return( randn );
}