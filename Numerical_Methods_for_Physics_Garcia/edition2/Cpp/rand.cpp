#include "NumMeth.h"

// Random number generator; Uniform dist. in [0,1)
double rand( long& seed ) {
// Input
//   seed    Integer seed (DO NOT USE A SEED OF ZERO)
// Output
//	 rand    Random number uniformly distributed in [0,1)

  const double a = 16807.0;
  const double m = 2147483647.0;
  double temp = a * seed;
  seed = (long)(fmod(temp,m));
  double rand = seed/m;
  return( rand );
}