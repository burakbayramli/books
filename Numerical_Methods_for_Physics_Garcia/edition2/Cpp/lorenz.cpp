// lorenz - Program to compute the trajectories of the Lorenz 
// equations using the adaptive Runge-Kutta method.
#include "NumMeth.h"

void lorzrk(double x[], double t, double param[], double deriv[]);
void rka( double x[], int nX, double& t, double& tau, double err,
  void (*derivsRK)(double x[], double t, double param[], double deriv[]), 
  double param[]);         
         
void main() {

  //* Set initial state x,y,z and parameters r,sigma,b
  cout << "Enter initial state (x,y,z)" << endl;
  double x; cout << "x = "; cin >> x;
  double y; cout << "y = "; cin >> y;
  double z; cout << "z = "; cin >> z;
  const int nState = 3;       // Number of elements in state
  double state[nState+1];
  state[1] = x;  state[2] = y;  state[3] = z;
  cout << "Enter the parameter r: ";
  double r;  cin >> r; 
  double sigma = 10.;   // Parameter sigma
  double b = 8./3.;     // Parameter b
  double param[3+1];      // Vector of parameters passed to rka
  param[1] = r;  param[2] = sigma; param[3] = b;
  double tau = 1.0;     // Initial guess for the timestep
  double err = 1.e-3;   // Error tolerance

  //* Loop over the desired number of steps
  double time = 0;
  cout << "Enter number of steps: ";
  int iStep, nStep;  cin >> nStep;
  double *tplot, *tauplot, *xplot, *yplot, *zplot;   
  tplot = new double [nStep+1];  tauplot = new double [nStep+1];
  xplot = new double [nStep+1];  // Plotting variables
  yplot = new double [nStep+1];  zplot = new double [nStep+1];
  for( iStep=1; iStep<=nStep; iStep++ ) {
  
    //* Record values for plotting
    x = state[1]; y = state[2]; z = state[3];
    tplot[iStep] = time;  tauplot[iStep] = tau;       
    xplot[iStep] = x;  yplot[iStep] = y;  zplot[iStep] = z;
    if( (iStep % 50) < 1 )
     cout << "Finished " << iStep << " steps out of "
          << nStep << endl;

    //* Find new state using adaptive Runge-Kutta
    rka(state,nState,time,tau,err,lorzrk,param);
  }

  //* Print max and min time step returned by rka
  double maxTau = tauplot[2], minTau = tauplot[2];
  int i;
  for( i=3; i<=nStep; i++ ) {
    maxTau = (maxTau > tauplot[i]) ? maxTau:tauplot[i];
    minTau = (minTau < tauplot[i]) ? minTau:tauplot[i];
  }
  cout << "Adaptive time step: Max = " << maxTau << 
                            "  Min = " << minTau << endl;

  // Find the location of the three steady states
  double x_ss[3+1], y_ss[3+1], z_ss[3+1];
  x_ss[1] = 0;              y_ss[1] = 0;       z_ss[1] = 0;
  x_ss[2] = sqrt(b*(r-1));  y_ss[2] = x_ss[2]; z_ss[2] = r-1;
  x_ss[3] = -sqrt(b*(r-1)); y_ss[3] = x_ss[3]; z_ss[3] = r-1;

  //* Print out the plotting variables: 
  //    tplot, xplot, yplot, zplot, x_ss, y_ss, z_ss
  ofstream tplotOut("tplot.txt"), xplotOut("xplot.txt"), 
	       yplotOut("yplot.txt"), zplotOut("zplot.txt"),
           x_ssOut("x_ss.txt"), y_ssOut("y_ss.txt"), 
		   z_ssOut("z_ss.txt");
  for( i=1; i<=nStep; i++ ) {
    tplotOut << tplot[i] << endl;
    xplotOut << xplot[i] << endl;
    yplotOut << yplot[i] << endl;
    zplotOut << zplot[i] << endl;
  }
  for( i=1; i<=3; i++ ) {
    x_ssOut << x_ss[i] << endl;
    y_ssOut << y_ss[i] << endl;
    z_ssOut << z_ss[i] << endl;
  }

  delete [] tplot, tauplot, xplot, yplot, zplot; // Release memory

}
/***** To plot in MATLAB; use the script below ********************
load tplot.txt; load xplot.txt; load yplot.txt; load zplot.txt;
load x_ss.txt; load y_ss.txt; load z_ss.txt;
%* Graph the time series x(t)
figure(1); clf;  % Clear figure 1 window and bring forward
plot(tplot,xplot,'-')
xlabel('Time');  ylabel('x(t)')
title('Lorenz model time series')
pause(1)  % Pause 1 second
%* Graph the x,y,z phase space trajectory
figure(2); clf;  % Clear figure 2 window and bring forward
plot3(xplot,yplot,zplot,'-',x_ss,y_ss,z_ss,'*')
view([30 20]);  % Rotate to get a better view 
grid;           % Add a grid to aid perspective
xlabel('x'); ylabel('y'); zlabel('z');
title('Lorenz model phase space');
******************************************************************/
