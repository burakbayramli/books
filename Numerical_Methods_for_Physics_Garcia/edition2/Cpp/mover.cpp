#include "NumMeth.h"

double rand( long& seed );
double randn( long& seed );

void mover( Matrix& x, Matrix& v, int npart, double L,
		    double mpv, double vwall, double tau,
			Matrix& strikes, Matrix& delv, long& seed ) {

// mover - Function to move particles by free flight
//         Also handles collisions with walls
// Inputs
//    x        Positions of the particles
//    v        Velocities of the particles
//    npart    Number of particles in the system
//    L        System length
//    mpv      Most probable velocity off the wall
//    vwall    Wall velocities
//    tau      Time step
//    seed     Random number seed
// Outputs
//    x,v      Updated positions and velocities
//    strikes  Number of particles striking each wall
//    delv     Change of y-velocity at each wall     
//    seed     Random number seed

  //* Move all particles pretending walls are absent
  Matrix x_old(npart);
  x_old = x;            // Remember original position
  int i;
  for( i=1; i<= npart; i++ )
    x(i) = x_old(i) + v(i,1)*tau;  

  //* Check each particle to see if it strikes a wall
  strikes.set(0.0);   delv.set(0.0);
  Matrix xwall(2), vw(2), direction(2);
  xwall(1) = 0;    xwall(2) = L;   // Positions of walls
  vw(1) = -vwall;  vw(2) = vwall;  // Velocities of walls
  double stdev = mpv/sqrt(2.);
  // Direction of particle leaving wall
  direction(1) = 1;  direction(2) = -1;
  for( i=1; i<=npart; i++ ) {

    //* Test if particle strikes either wall
	int flag = 0;
    if( x(i) <= 0 )
      flag=1;       // Particle strikes left wall
    else if( x(i) >= L )
      flag=2;       // Particle strikes right wall

    //* If particle strikes a wall, reset its position
    //  and velocity. Record velocity change.
    if( flag > 0 )	{
      strikes(flag)++;
      double vyInitial = v(i,2);
      //* Reset velocity components as biased Maxwellian,
      //  Exponential dist. in x; Gaussian in y and z
      v(i,1) = direction(flag)*sqrt(-log(1.-rand(seed))) * mpv;
      v(i,2) = stdev*randn(seed) + vw(flag); // Add wall velocity
      v(i,3) = stdev*randn(seed);
      // Time of flight after leaving wall
      double dtr = tau*(x(i)-xwall(flag))/(x(i)-x_old(i));   
      //* Reset position after leaving wall
      x(i) = xwall(flag) + v(i,1)*dtr;
      //* Record velocity change for force measurement
      delv(flag) += (v(i,2) - vyInitial);
    }
  }
}
