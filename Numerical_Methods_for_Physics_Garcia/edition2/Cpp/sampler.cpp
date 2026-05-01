#include "NumMeth.h"
#include "SampList.h"

void sampler( Matrix& x, Matrix& v, int npart, double L,
		    SampList& sampD ) {

// sampler - Function to sample density, velocity and temperature
// Inputs
//    x       Particle positions
//    v       Particle velocities
//    npart   Number of particles
//    L       System size
//    sampD   Object with sampling data
// Outputs
//    sampD   Structure with sampling data

  //* Compute cell location for each particle
  int ncell = sampD.ncell;
  int *jx; jx = new int [npart+1];
  int i;
  for( i=1; i<=npart; i++ )	
    jx[i] = (int)ceil(ncell*x(i)/L);

  //* Initialize running sums of number, velocity and v^2
  Matrix sum_n(ncell), sum_vx(ncell), sum_vy(ncell),
	     sum_vz(ncell), sum_v2(ncell);
  sum_n.set(0.0);
  sum_vx.set(0.0);
  sum_vy.set(0.0);
  sum_vz.set(0.0);
  sum_v2.set(0.0);

  //* For each particle, accumulate running sums for its cell
  for( i=1; i<=npart; i++ )	{
    int jcell = jx[i];  // Particle i is in cell jcell
    sum_n(jcell)++;
    sum_vx(jcell) += v(i,1);
    sum_vy(jcell) += v(i,2);
    sum_vz(jcell) += v(i,3);
    sum_v2(jcell) += v(i,1)*v(i,1) + 
		             v(i,2)*v(i,2) + v(i,3)*v(i,3);
}

  //* Use current sums to update sample number, velocity 
  //  and temperature
  for( i=1; i<=ncell; i++ ) {
    sum_vx(i) /= sum_n(i);
    sum_vy(i) /= sum_n(i);
    sum_vz(i) /= sum_n(i);
    sum_v2(i) /= sum_n(i);
    sampD.ave_n[i] += sum_n(i);
    sampD.ave_ux[i] += sum_vx(i);
    sampD.ave_uy[i] += sum_vy(i);
    sampD.ave_uz[i] += sum_vz(i);
    sampD.ave_T[i] += sum_v2(i) - (sum_vx(i)*sum_vx(i) + 
		  sum_vy(i)*sum_vy(i) + sum_vz(i)*sum_vz(i));
  }
  sampD.nsamp++;

  delete [] jx;
}
