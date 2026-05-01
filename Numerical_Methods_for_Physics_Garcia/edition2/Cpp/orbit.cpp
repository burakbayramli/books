// orbit - Program to compute the orbit of a comet.
#include "NumMeth.h"

void gravrk( double x[], double t, double param[], double deriv[] );
void rk4( double x[], int nX, double t, double tau, 
  void (*derivsRK)(double x[], double t, double param[], double deriv[]), 
  double param[]);
void rka( double x[], int nX, double& t, double& tau, double err,
  void (*derivsRK)(double x[], double t, double param[], double deriv[]), 
  double param[]);         
         
void main() {

  //* Set initial position and velocity of the comet.
  double r0, v0;
  cout << "Enter initial radial distance (AU): "; cin >> r0;  
  cout << "Enter initial tangential velocity (AU/yr): "; cin >> v0;
  double r[2+1], v[2+1], state[4+1], accel[2+1];
  r[1] = r0; r[2] = 0;  v[1] = 0; v[2] = v0;
  state[1] = r[1]; state[2] = r[2];   // Used by R-K routines
  state[3] = v[1]; state[4] = v[2];
  int nState = 4;  // Number of elements in state vector

  //* Set physical parameters (mass, G*M)
  const double pi = 3.141592654;
  double GM = 4*pi*pi;      // Grav. const. * Mass of Sun (au^3/yr^2)
  double param[1+1];  param[1] = GM;
  double mass = 1.;         // Mass of comet 
  double adaptErr = 1.e-3;  // Error parameter used by adaptive Runge-Kutta
  double time = 0;

  //* Loop over desired number of steps using specified
  //  numerical method.
  cout << "Enter number of steps: "; 
  int nStep;  cin >> nStep;
  cout << "Enter time step (yr): ";
  double tau;  cin >> tau; 
  cout << "Choose a numerical method:" << endl;
  cout << "1) Euler,       2) Euler-Cromer, "	<< endl
	   << "3) Runge-Kutta, 4) Adaptive R-K: ";
  int method;  cin >> method;
  double *rplot, *thplot, *tplot, *kinetic, *potential;  // Plotting variables
  rplot = new double [nStep+1];  thplot = new double [nStep+1];
  tplot = new double [nStep+1];
  kinetic = new double [nStep+1]; potential = new double [nStep+1];
  int iStep;
  for( iStep=1; iStep<=nStep; iStep++ ) {  

    //* Record position and energy for plotting.
    double normR = sqrt( r[1]*r[1] + r[2]*r[2] );
    double normV = sqrt( v[1]*v[1] + v[2]*v[2] );
    rplot[iStep] = normR;               // Record position for plotting
    thplot[iStep] = atan2(r[2],r[1]);
    tplot[iStep] = time;
    kinetic[iStep] = 0.5*mass*normV*normV;   // Record energies
    potential[iStep] = - GM*mass/normR;
  
    //* Calculate new position and velocity using desired method.
    if( method == 1 ) {
      accel[1] = -GM*r[1]/(normR*normR*normR);   
      accel[2] = -GM*r[2]/(normR*normR*normR);   
      r[1] += tau*v[1];             // Euler step
      r[2] += tau*v[2];             
      v[1] += tau*accel[1]; 
      v[2] += tau*accel[2]; 
      time += tau;  
    } 
    else if( method == 2 ) {
      accel[1] = -GM*r[1]/(normR*normR*normR);   
      accel[2] = -GM*r[2]/(normR*normR*normR);   
      v[1] += tau*accel[1]; 
      v[2] += tau*accel[2]; 
      r[1] += tau*v[1];             // Euler-Cromer step
      r[2] += tau*v[2];             
      time += tau;  
    }
    else if( method == 3 ) {
      rk4( state, nState, time, tau, gravrk, param );
      r[1] = state[1]; r[2] = state[2];   // 4th order Runge-Kutta
      v[1] = state[3]; v[2] = state[4];
      time += tau;   
    }
    else {
      rka( state, nState, time, tau, adaptErr, gravrk, param );
      r[1] = state[1]; r[2] = state[2];   // Adaptive Runge-Kutta
      v[1] = state[3]; v[2] = state[4];
    }
  
  }

  //* Print out the plotting variables: 
  //    thplot, rplot, potential, kinetic
  ofstream thplotOut("thplot.txt"), rplotOut("rplot.txt"),
	   tplotOut("tplot.txt"), potentialOut("potential.txt"), 
	   kineticOut("kinetic.txt");
  int i;
  for( i=1; i<=nStep; i++ ) {
    thplotOut << thplot[i] << endl;
    rplotOut << rplot[i] << endl;
    tplotOut << tplot[i] << endl;
    potentialOut << potential[i] << endl;
    kineticOut << kinetic[i] << endl;
  }

  delete [] rplot, thplot, tplot, kinetic, potential;

}
/***** To plot in MATLAB; use the script below ********************
load thplot.txt; load rplot.txt; load tplot.txt;
load potential.txt; load kinetic.txt;
%* Graph the trajectory of the comet.
figure(1); clf;  % Clear figure 1 window and bring forward
polar(thplot,rplot,'+');  % Use polar plot for graphing orbit
xlabel('Distance (AU)');  grid;
pause(1)   % Pause for 1 second before drawing next plot
%* Graph the energy of the comet versus time.
figure(2); clf;   % Clear figure 2 window and bring forward
totalE = kinetic + potential;   % Total energy
plot(tplot,kinetic,'-.',tplot,potential,'--',tplot,totalE,'-')
legend('Kinetic','Potential','Total');
xlabel('Time (yr)'); ylabel('Energy (M AU^2/yr^2)');
******************************************************************/
