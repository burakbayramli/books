// traffic - Program to solve the generalized Burger  
// equation for the traffic at a stop light problem
#include "NumMeth.h"
         
void main() {

  //* Select numerical parameters (time step, grid spacing, etc.).
  cout << "Choose a numerical method: 1) FTCS, 2) Lax, 3) Lax-Wendroff : ";
  int method; cin >> method;
  cout << "Enter the number of grid points: "; int N; cin >> N;
  double L = 400;      // System size (meters)
  double h = L/N;      // Grid spacing for periodic boundary conditions
  double v_max = 25;   // Maximum car speed (m/s)
  cout << "Suggested timestep is " << h/v_max << endl;
  cout << "Enter time step (tau): "; double tau; cin >> tau;
  cout << "Last car starts moving after " 
       << (L/4)/(v_max*tau) << " steps" << endl;
  cout << "Enter number of steps: "; int nStep; cin >> nStep;
  double coeff = tau/(2*h);          // Coefficient used by all schemes
  double coefflw = tau*tau/(2*h*h);  // Coefficient used by Lax-Wendroff
  double cp, cm;       // Variables used by Lax-Wendroff

  //* Set initial and boundary conditions
  double rho_max = 1.0;                  // Maximum density
  double Flow_max = 0.25*rho_max*v_max;  // Maximum Flow
  // Initial condition is a square pulse from x = -L/4 to x = 0
  Matrix rho(N), rho_new(N);
  int i,j, iBack = N/4, iFront = N/2 - 1;
  for( i=1; i<=N; i++ )
    if( iBack <= i && i <= iFront ) rho(i) = rho_max;
    else rho(i) = 0.0;
  rho(iFront+1) = rho_max/2;  // Try running without this line
  // Use periodic boundary conditions
  int *ip, *im;  ip = new int [N+1];  im = new int [N+1];
  for( i=2; i<N; i++ ) {
    ip[i] = i+1;    // ip[i] = i+1 with periodic b.c.
    im[i] = i-1;    // im[i] = i-1 with periodic b.c.
  }
  ip[1] = 2;  ip[N] = 1;
  im[1] = N;  im[N] = N-1;

  //* Initialize plotting variables.
  int iplot = 1;
  Matrix tplot(nStep+1), xplot(N), rplot(N,nStep+1);
  tplot(1) = 0.0;             // Record initial time
  for( i=1; i<=N; i++ ) {    
    xplot(i) = (i - 0.5)*h - L/2;  // Record x scale for plot
    rplot(i,1) = rho(i);           // Record the initial state
  }
  
  //* Loop over desired number of steps.
  Matrix Flow(N);
  int iStep;
  for( iStep=1; iStep<=nStep; iStep++ ) {
	
    //* Compute the flow = (Density)*(Velocity)
    for( i=1; i<=N; i++ )
      Flow(i) = rho(i) * (v_max*(1.0 - rho(i)/rho_max));
  
    //* Compute new values of density using FTCS, 
    //  Lax or Lax-Wendroff method.
    if( method == 1 )       ////// FTCS method //////
      for( i=1; i<=N; i++ )
        rho_new(i) = rho(i) - coeff*(Flow(ip[i])-Flow(im[i]));
    else if( method == 2 )  ////// Lax method //////
      for( i=1; i<=N; i++ )
        rho_new(i) = 0.5*(rho(ip[i])+rho(im[i]))
                   - coeff*(Flow(ip[i])-Flow(im[i]));
    else                    ////// Lax-Wendroff method //////
      for( i=1; i<=N; i++ ) {
        cp = v_max*(1 - (rho(ip[i])+rho(i))/rho_max);
        cm = v_max*(1 - (rho(i)+rho(im[i]))/rho_max);
        rho_new(i) = rho(i) - coeff*(Flow(ip[i])-Flow(im[i]))
             + coefflw*(cp*(Flow(ip[i])-Flow(i)) 
                      - cm*(Flow(i)-Flow(im[i])));
      }
    // Reset with new density values  
    rho = rho_new;

    //* Record density for plotting.
    cout << "Finished " << iStep << " of " << nStep << " steps" << endl;
    iplot++;
    tplot(iplot) = tau*iStep;
    for( i=1; i<=N; i++ ) 
      rplot(i,iplot) = rho(i);
  
  }
  int nplots = iplot;     // Number of plots recorded
  
  //* Print out the plotting variables: tplot, xplot, rplot
  ofstream tplotOut("tplot.txt"), xplotOut("xplot.txt"), 
	      rplotOut("rplot.txt");
  for( i=1; i<=nplots; i++ ) 
    tplotOut << tplot(i) << endl;
  for( i=1; i<=N; i++ ) {
    xplotOut << xplot(i) << endl;
    for( j=1; j<nplots; j++ )
      rplotOut << rplot(i,j) << ", ";
    rplotOut << rplot(i,nplots) << endl;
  }

  delete [] ip, im;
}
/***** To plot in MATLAB; use the script below ********************
load tplot.txt; load xplot.txt; load rplot.txt;
%* Graph density versus position and time as wire-mesh plot
figure(1); clf;  % Clear figure 1 window and bring forward
mesh(tplot,xplot,rplot)
xlabel('t'); ylabel('x'); zlabel('\rho');
title('Density versus position and time');
view([100 30]);  % Rotate the plot for better view point
pause(1);    % Pause 1 second between plots
%* Graph contours of density versus position and time.
figure(2); clf;   % Clear figure 2 window and bring forward
% Use rot90 function to graph t vs x since
% contour(rplot) graphs x vs t.
clevels = 0:(0.1):1;   % Contour levels
cs = contour(xplot,tplot,flipud(rot90(rplot)),clevels); 
clabel(cs);            % Put labels on contour levels            
xlabel('x');  ylabel('time');  title('Density contours');
******************************************************************/


