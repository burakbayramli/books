// pendul - Program to compute the motion of a simple pendulum
// using the Euler or Verlet method
#include "NumMeth.h"

void main() {

  //* Select the numerical method to use: Euler or Verlet
  cout << "Choose a numerical method 1) Euler, 2) Verlet: ";
  int method; cin >> method;
					   
  //* Set initial position and velocity of pendulum
  cout << "Enter initial angle (in degrees): "; 
  double theta0; cin >> theta0;
  const double pi = 3.141592654;
  double theta = theta0*pi/180;   // Convert angle to radians
  double omega = 0.0;             // Set the initial velocity

  //* Set the physical constants and other variables
  double g_over_L = 1.0;          // The constant g/L
  double time = 0.0;              // Initial time
  double time_old;                // Time of previous reversal
  int irev = 0;                   // Used to count number of reversals
  cout << "Enter time step: ";
  double tau; cin >> tau;

  //* Take one backward step to start Verlet
  double accel = -g_over_L*sin(theta);    // Gravitational acceleration
  double theta_old = theta - omega*tau + 0.5*tau*tau*accel;    

  //* Loop over desired number of steps with given time step
  //    and numerical method
  cout << "Enter number of time steps: ";
  int nStep;  cin >> nStep;
  double *t_plot, *th_plot, *period;  // Plotting variables
  t_plot = new double [nStep+1]; th_plot = new double [nStep+1];
  period = new double [nStep+1];
  int iStep;
  for( iStep=1; iStep<=nStep; iStep++ ) {  

    //* Record angle and time for plotting
    t_plot[iStep] = time;            
    th_plot[iStep] = theta*180/pi;   // Convert angle to degrees
    time += tau;
  
    //* Compute new position and velocity using 
    //    Euler or Verlet method
    accel = -g_over_L*sin(theta);    // Gravitational acceleration
    if( method == 1 ) {
      theta_old = theta;        // Save previous angle
      theta += tau*omega;       // Euler method
      omega += tau*accel; 
    }
    else {  
      double theta_new = 2*theta - theta_old + tau*tau*accel;
      theta_old = theta;	    // Verlet method
      theta = theta_new;  
    }
  
    //* Test if the pendulum has passed through theta = 0;
    //    if yes, use time to estimate period
    if( theta*theta_old < 0 ) { // Test position for sign change
      cout << "Turning point at time t = " << time << endl;
      if( irev == 0 )          // If this is the first change,
        time_old = time;       // just record the time
      else {
        period[irev] = 2*(time - time_old);
        time_old = time;
      }
      irev++;       // Increment the number of reversals
    }
  }
  int nPeriod = irev-1;    // Number of times period is measured

  //* Estimate period of oscillation, including error bar
  double AvePeriod = 0.0, ErrorBar = 0.0;
  int i;
  for( i=1; i<=nPeriod; i++ ) {
    AvePeriod += period[i];
  }
  AvePeriod /= nPeriod;
  for( i=1; i<=nPeriod; i++ ) {
    ErrorBar += (period[i] - AvePeriod)*(period[i] - AvePeriod);
  }
  ErrorBar = sqrt(ErrorBar/(nPeriod*(nPeriod-1)));
  cout << "Average period = " << AvePeriod << " +/- " << ErrorBar << endl;

  //* Print out the plotting variables: t_plot, th_plot
  ofstream t_plotOut("t_plot.txt"), th_plotOut("th_plot.txt");
  for( i=1; i<=nStep; i++ ) {
    t_plotOut << t_plot[i] << endl;
    th_plotOut << th_plot[i] << endl;
  }

  delete [] t_plot, th_plot, period;

}
/***** To plot in MATLAB; use the script below ********************
load t_plot.txt; load th_plot.txt;
clf;  figure(gcf);         % Clear and forward figure window
plot(t_plot,th_plot,'+');
xlabel('Time');  ylabel('Theta (degrees)');
******************************************************************/
