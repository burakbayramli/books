// dsmcne - Program to simulate a dilute gas using DSMC algorithm
// This version simulates planar Couette flow

#include "NumMeth.h"
#include "SortList.h"
#include "SampList.h"

double rand( long& seed );
double randn( long& seed );
int colider( Matrix& v, Matrix& crmax, double tau, long& seed,
              Matrix& selxtra, double coeff, SortList& sD );
void sorter( Matrix& x, double L, SortList &sD );
void mover( Matrix& x, Matrix& v, int npart, double L,
            double mpv, double vwall, double tau,
            Matrix& strikes, Matrix& delv, long& seed );
void sampler( Matrix& x, Matrix& v, int npart, double L,
            SampList& sampD );

void main() {

  //* Initialize constants  (particle mass, diameter, etc.)
  const double pi = 3.141592654;
  const double boltz = 1.3806e-23;    // Boltzmann's constant (J/K)
  double mass = 6.63e-26;       // Mass of argon atom (kg)
  double diam = 3.66e-10;       // Effective diameter of argon atom (m)
  double T = 273;               // Temperature (K)
  double density = 2.685e25;    // Number density of argon at STP (m^-3)
  double L = 1e-6;              // System size is one micron
  double Volume = L*L*L;        // Volume of the system
  cout << "Enter number of simulation particles: ";
  int npart; cin >> npart;
  double eff_num = density*L*L*L/npart;
  cout << "Each particle represents " << eff_num << " atoms" << endl;
  double mfp = Volume/(sqrt(2.0)*pi*diam*diam*npart*eff_num);
  cout << "System width is " << L/mfp << " mean free paths" << endl;
  double mpv = sqrt(2*boltz*T/mass);  // Most probable initial velocity
  cout << "Enter wall velocity as Mach number: ";
  double vwall_m; cin >> vwall_m;
  double vwall = vwall_m * sqrt(5./3. * boltz*T/mass);
  cout << "Wall velocities are " << -vwall << " and "
       << vwall << " m/s" << endl;

  //* Assign random positions and velocities to particles
  long seed = 1;       // Initial seed for rand (DO NOT USE ZERO)
  Matrix x(npart), v(npart,3);
  int i;
  for( i=1; i<=npart; i++ ) {
    x(i) = L*rand(seed);        // Assign random positions
    // Initial velocities are Maxwell-Boltzmann distributed
    v(i,1) = sqrt(boltz*T/mass) * randn(seed);
    v(i,2) = sqrt(boltz*T/mass) * randn(seed);
    v(i,3) = sqrt(boltz*T/mass) * randn(seed);
    // Add velocity gradient to the y-component
    v(i,2) += vwall * 2*(x(i)/L - 0.5);
  }

  //* Initialize variables used for evaluating collisions
  int ncell = 20;                       // Number of cells
  double tau = 0.2*(L/ncell)/mpv;       // Set timestep tau
  Matrix vrmax(ncell), selxtra(ncell);
  vrmax.set(3*mpv);    // Estimated max rel. speed
  selxtra.set(0.0);       // Used by routine "colider"
  double coeff = 0.5*eff_num*pi*diam*diam*tau/(L*L*L/ncell);

  //* Declare object for lists used in sorting
  SortList sortData(ncell,npart);

  //* Initialize object and variables used in statistical sampling
  SampList sampData(ncell);
  double tsamp = 0;             // Total sampling time
  Matrix dvtot(2), dverr(2);
  dvtot.set(0.0);               // Total momentum change at a wall
  dverr.set(0.0);               // Used to find error in dvtot

  //* Loop for the desired number of time steps
  int colSum = 0;         // Count total collisions
  Matrix strikes(2), strikeSum(2);
  strikeSum.set(0.0);     // Count strikes on each wall
  cout << "Enter total number of time steps: ";
  int istep, nstep; cin >> nstep;
  for( istep = 1; istep<=nstep; istep++ ) {

    //* Move all the particles
    Matrix delv(2);  delv.set(0.0);
    mover( x, v, npart, L, mpv, vwall,
           tau, strikes, delv, seed );
    strikeSum(1) += strikes(1);
    strikeSum(2) += strikes(2);

    //* Sort the particles into cells
    sorter(x,L,sortData);

    //* Evaluate collisions among the particles
    int col = colider(v,vrmax,tau,seed,selxtra,coeff,sortData);
    colSum += col;  // Increment collision count

    //* After initial transient, accumulate statistical samples
    if(istep > nstep/10) {
      sampler(x,v,npart,L,sampData);
      // Cummulative velocity change for particles striking walls
      dvtot(1) += delv(1);          dvtot(2) += delv(2);
      dverr(1) += delv(1)*delv(1);  dverr(2) += delv(2)*delv(2);
      tsamp += tau;
    }

    //* Periodically display the current progress
    if( (istep%100) < 1 )  {
      cout << "Done " << istep << " of " << nstep << " steps; " <<
             colSum << " collisions" << endl;
      cout << "Total wall strikes: " << strikeSum(1) << " (left) "
           << strikeSum(2) << " (right)" << endl;
    }
  }

  //* Normalize the accumulated statistics
  int nsamp = sampData.nsamp;
  for( i=1; i<=ncell; i++ ) {
    sampData.ave_n[i] *= (eff_num/(Volume/ncell))/nsamp;
    sampData.ave_ux[i] /= nsamp;
    sampData.ave_uy[i] /= nsamp;
    sampData.ave_uz[i] /= nsamp;
    sampData.ave_T[i] *= mass/(3*boltz*nsamp);
  }
  dverr(1) = dverr(1)/(nsamp-1) - (dvtot(1)/nsamp)*(dvtot(1)/nsamp);
  dverr(1) = sqrt(dverr(1)*nsamp);
  dverr(2) = dverr(2)/(nsamp-1) - (dvtot(2)/nsamp)*(dvtot(2)/nsamp);
  dverr(2) = sqrt(dverr(2)*nsamp);

  //* Compute viscosity from drag force on the walls
  Matrix force(2), ferr(2);
  force(1) = (eff_num*mass*dvtot(1))/(tsamp*L*L);
  force(2) = (eff_num*mass*dvtot(2))/(tsamp*L*L);
  ferr(1) = (eff_num*mass*dverr(1))/(tsamp*L*L);
  ferr(2) = (eff_num*mass*dverr(2))/(tsamp*L*L);
  cout << "Force per unit area is" << endl;
  cout << "Left wall:  " << force(1) << " +/- " << ferr(1) << endl;
  cout << "Right wall: " << force(2) << " +/- " << ferr(2) << endl;
  double vgrad = 2*vwall/L;    // Velocity gradient
  double visc = 0.5*(-force(1)+force(2))/vgrad;  // Average viscosity
  double viscerr = 0.5*(ferr(1)+ferr(2))/vgrad;  // Error
  cout << "Viscosity = " << visc << " +/- " << viscerr
       << "N s/m^2" << endl;
  double eta = 5.*pi/32.*mass*density*(2./sqrt(pi)*mpv)*mfp;
  cout << "Theoretical value of viscoisty is " << eta
       << "N s/m^2" << endl;


  //* Print out the plotting variables:
  //    xcell, ave_n, ave_ux, ave_uy, ave_uz, ave_T
  ofstream xcellOut("xcell.txt"), ave_nOut("ave_n.txt"),
           ave_uxOut("ave_ux.txt"), ave_uyOut("ave_uy.txt"),
           ave_uzOut("ave_uz.txt"), ave_TOut("ave_T.txt");
  for( i=1; i<=ncell; i++ ) {
    xcellOut << (i-0.5)*L/ncell << endl;
    ave_nOut << sampData.ave_n[i] << endl;
    ave_uxOut << sampData.ave_ux[i] << endl;
    ave_uyOut << sampData.ave_uy[i] << endl;
    ave_uzOut << sampData.ave_uz[i] << endl;
    ave_TOut << sampData.ave_T[i] << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load xcell.txt;  load ave_n.txt;     load ave_ux.txt;
load ave_uy.txt; load ave_uz.txt;    load ave_T.txt;
figure(1); clf;
plot(xcell,ave_n); xlabel('position');  ylabel('Number density');
figure(2); clf;
plot(xcell,ave_ux,xcell,ave_uy,xcell,ave_uz);
xlabel('position');  ylabel('Velocities');
legend('x-component','y-component','z-component');
figure(3); clf;
plot(xcell,ave_T); xlabel('position');  ylabel('Temperature');
******************************************************************/
