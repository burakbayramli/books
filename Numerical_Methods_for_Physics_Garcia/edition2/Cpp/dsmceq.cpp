//  dsmceq - Dilute gas simulation using DSMC algorithm
//  This version illustrates the approach to equilibrium

#include "NumMeth.h"
#include "SortList.h"         
 
double rand( long& seed );
int colider( Matrix& v, Matrix& crmax, double tau, long& seed,
			  Matrix& selxtra, double coeff, SortList& sD );
void sorter( Matrix& x, double L, SortList &sD );
			  
void main() {

  //* Initialize constants  (particle mass, diameter, etc.)
  const double pi = 3.141592654;
  const double boltz = 1.3806e-23;    // Boltzmann's constant (J/K)
  double mass = 6.63e-26;       // Mass of argon atom (kg)
  double diam = 3.66e-10;       // Effective diameter of argon atom (m)
  double T = 273;               // Temperature (K)
  double density = 1.78;        // Density of argon at STP (kg/m^3)
  double L = 1e-6;              // System size is one micron
  cout << "Enter number of simulation particles: "; 
  int npart; cin >> npart;
  double eff_num = density/mass*L*L*L/npart;
  cout << "Each particle represents " << eff_num << " atoms" << endl;

  //* Assign random positions and velocities to particles
  long seed = 1;       // Initial seed for rand (DO NOT USE ZERO)
  double v_init = sqrt(3.0*boltz*T/mass);    // Initial speed
  Matrix x(npart), v(npart,3);
  int i;
  for( i=1; i<=npart; i++ ) {
    x(i) = L*rand(seed);   // Assign random positions
	int plusMinus = (1 - 2*((int)(2*rand(seed))));
    v(i,1) = plusMinus * v_init;
	v(i,2) = 0.0;	// Only x-component is non-zero
	v(i,3) = 0.0;
  }

  //* Record inital particle speeds
  Matrix vmagI(npart);
  for( i=1; i<=npart; i++ )
	vmagI(i) = sqrt( v(i,1)*v(i,1) + v(i,2)*v(i,2) + v(i,3)*v(i,3) );

  //* Initialize variables used for evaluating collisions
  int ncell = 15;                       // Number of cells
  double tau = 0.2*(L/ncell)/v_init;    // Set timestep tau
  Matrix vrmax(ncell), selxtra(ncell);
  vrmax.set(3*v_init);    // Estimated max rel. speed
  selxtra.set(0.0);       // Used by routine "colider"
  double coeff = 0.5*eff_num*pi*diam*diam*tau/(L*L*L/ncell);
  int coltot = 0;         // Count total collisions

  //* Declare object for lists used in sorting
  SortList sortData(ncell,npart);  

  //* Loop for the desired number of time steps
  cout << "Enter total number of time steps: ";
  int istep, nstep; cin >> nstep;
  for( istep = 1; istep<=nstep; istep++ ) {
	
    //* Move all the particles ballistically
	for( i=1; i<=npart; i++ ) {
      x(i) += v(i,1)*tau;          // Update x position of particle
      x(i) = fmod(x(i)+L,L);       // Periodic boundary conditions
	}
    //* Sort the particles into cells
    sorter(x,L,sortData);
  
    //* Evaluate collisions among the particles
    int col = colider(v,vrmax,tau,seed,selxtra,coeff,sortData);
    coltot += col;	// Increment collision count
    
    //* Periodically display the current progress
    if( (istep%10) < 1 )
      cout << "Done " << istep << " of " << nstep << " steps; " << 
	         coltot << " collisions" << endl;
  }

  // Record final particle speeds
  Matrix vmagF(npart);
  for( i=1; i<=npart; i++ )
	vmagF(i) = sqrt( v(i,1)*v(i,1) + v(i,2)*v(i,2) + v(i,3)*v(i,3) );

  //* Print out the plotting variables: vmagI, vmagF
  ofstream vmagIOut("vmagI.txt"), vmagFOut("vmagF.txt");
  for( i=1; i<=npart; i++ ) {
    vmagIOut << vmagI(i) << endl;
    vmagFOut << vmagF(i) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load vmagI.txt; load vmagF.txt;
%* Plot the histogram of the initial speed distribution
vbin = 50:100:1050;    % Bins for histogram
hist(vmagI,vbin);  title('Initial speed distribution');
xlabel('Speed (m/s)');  ylabel('Number');
%* Plot the histogram of the final speed distribution
figure(2); clf;
hist(vmagF,vbin);  
title(sprintf('Final speed distribution'));
xlabel('Speed (m/s)');  ylabel('Number');
******************************************************************/
   
