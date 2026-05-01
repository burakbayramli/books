#include "NumMeth.h"
#include "SortList.h"

double rand( long& seed );

int colider( Matrix& v, Matrix& crmax, double tau, long& seed,
              Matrix& selxtra, double coeff, SortList& sD ) {

// colide - Function to process collisions in cells
// Inputs
//    v         Velocities of the particles
//    crmax     Estimated maximum relative speed in a cell
//    tau       Time step
//    seed      Current random number seed
//    selxtra   Extra selections carried over from last timestep
//    coeff     Coefficient in computing number of selected pairs
//    sD        Object containing sorting lists
// Outputs
//    v         Updated velocities of the particles
//    crmax     Updated maximum relative speed
//    selxtra   Extra selections carried over to next timestep
//    col       Total number of collisions processed    (Return value)

  int ncell = sD.ncell;
  int col = 0;          // Count number of collisions
  const double pi = 3.141592654;

  //* Loop over cells, processing collisions in each cell
  int jcell;
  for( jcell=1; jcell<=ncell; jcell++ ) {

    //* Skip cells with only one particle
    int number = sD.cell_n[jcell];
    if( number < 2 ) continue;  // Skip to the next cell

    //* Determine number of candidate collision pairs
    //  to be selected in this cell
    double select = coeff*number*number*crmax(jcell) + selxtra(jcell);
    int nsel = (int)(select);      // Number of pairs to be selected
    selxtra(jcell) = select-nsel;  // Carry over any left-over fraction
    double crm = crmax(jcell);     // Current maximum relative speed

    //* Loop over total number of candidate collision pairs
    int isel;
    for( isel=1; isel<=nsel; isel++ ) {

      //* Pick two particles at random out of this cell
      int k = (int)(rand(seed)*number);
      int kk = ((int)(k+rand(seed)*(number-1))+1) % number;
      int ip1 = sD.Xref[ k+sD.index[jcell] ];      // First particle
      int ip2 = sD.Xref[ kk+sD.index[jcell] ];     // Second particle

      //* Calculate pair's relative speed
      double cr = sqrt( pow(v(ip1,1)-v(ip2,1),2) +
                        pow(v(ip1,2)-v(ip2,2),2) +   // Relative speed
                        pow(v(ip1,3)-v(ip2,3),2) );
      if( cr > crm )         // If relative speed larger than crm,
        crm = cr;            // then reset crm to larger value

      //* Accept or reject candidate pair according to relative speed
      if( cr/crmax(jcell) > rand(seed) ) {
        //* If pair accepted, select post-collision velocities
        col++;                     // Collision counter
        Matrix vcm(3), vrel(3);
        int k;
        for( k=1; k<=3; k++ )
          vcm(k) = 0.5*(v(ip1,k) + v(ip2,k));      // Center of mass velocity
        double cos_th = 1.0 - 2.0*rand(seed);      // Cosine and sine of
        double sin_th = sqrt(1.0 - cos_th*cos_th); // collision angle theta
        double phi = 2.0*pi*rand(seed);            // Collision angle phi
        vrel(1) = cr*cos_th;             // Compute post-collision
        vrel(2) = cr*sin_th*cos(phi);    // relative velocity
        vrel(3) = cr*sin_th*sin(phi);
        for(  k=1; k<=3; k++ ) {
          v(ip1,k) = vcm(k) + 0.5*vrel(k);  // Update post-collision
          v(ip2,k) = vcm(k) - 0.5*vrel(k);  // velocities
        }

      } // Loop over pairs
      crmax(jcell) = crm;     // Update max relative speed
    }
  }   // Loop over cells
  return( col );
}
