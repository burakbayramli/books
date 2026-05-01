// dftcs - Program to solve the diffusion equation 
// using the Forward Time Centered Space (FTCS) scheme.
#include "NumMeth.h"
         
void main() {

  //* Initialize parameters (time step, grid spacing, etc.).
  cout << "Enter time step: "; double tau; cin >> tau;
  cout << "Enter the number of grid points: "; int N; cin >> N;
  double L = 1.;  // The system extends from x=-L/2 to x=L/2
  double h = L/(N-1);  // Grid size
  double kappa = 1.;   // Diffusion coefficient
  double coeff = kappa*tau/(h*h);
  if( coeff < 0.5 )
    cout << "Solution is expected to be stable" << endl;
  else
    cout << "WARNING: Solution is expected to be unstable" << endl;

  //* Set initial and boundary conditions.
  Matrix tt(N), tt_new(N);
  tt.set(0.0);     // Initialize temperature to zero at all points
  tt(N/2) = 1/h;   // Initial cond. is delta function in center
  //// The boundary conditions are tt(1) = tt(N) = 0
  tt_new.set(0.0);  // End points are unchanged during iteration

  //* Set up loop and plot variables.
  int iplot = 1;                 // Counter used to count plots
  int nStep = 300;               // Maximum number of iterations
  int plot_step = 6;             // Number of time steps between plots
  int nplots = nStep/plot_step + 1;  // Number of snapshots (plots)
  Matrix xplot(N), tplot(nplots), ttplot(N,nplots);
  int i,j;
  for( i=1; i<=N; i++ )
    xplot(i) = (i-1)*h - L/2;   // Record the x scale for plots

  //* Loop over the desired number of time steps.
  int iStep;
  for( iStep=1; iStep<=nStep; iStep++ ) {

    //* Compute new temperature using FTCS scheme.
    for( i=2; i<=(N-1); i++ )
      tt_new(i) = tt(i) + coeff*(tt(i+1) + tt(i-1) - 2*tt(i));
    
    tt = tt_new;     // Reset temperature to new values
  
    //* Periodically record temperature for plotting.
    if( (iStep%plot_step) < 1 ) { // Every plot_step steps
      for( i=1; i<=N; i++ )      // record tt(i) for plotting
        ttplot(i,iplot) = tt(i);     
      tplot(iplot) = iStep*tau;      // Record time for plots
      iplot++;
    }
  }
  nplots = iplot-1;  // Number of plots actually recorded
  
  //* Print out the plotting variables: tplot, xplot, ttplot
  ofstream tplotOut("tplot.txt"), xplotOut("xplot.txt"), 
	      ttplotOut("ttplot.txt");
  for( i=1; i<=nplots; i++ ) 
    tplotOut << tplot(i) << endl;
  for( i=1; i<=N; i++ ) {
    xplotOut << xplot(i) << endl;
    for( j=1; j<nplots; j++ )
      ttplotOut << ttplot(i,j) << ", ";
    ttplotOut << ttplot(i,nplots) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load tplot.txt; load xplot.txt; load ttplot.txt;
%* Plot temperature versus x and t as wire-mesh and contour plots.
figure(1); clf;
mesh(tplot,xplot,ttplot);  % Wire-mesh surface plot
xlabel('Time');  ylabel('x');  zlabel('T(x,t)');
title('Diffusion of a delta spike');
pause(1);
figure(2); clf;       
contourLevels = 0:0.5:10;  contourLabels = 0:5;     
cs = contour(tplot,xplot,ttplot,contourLevels);  % Contour plot
clabel(cs,contourLabels);  % Add labels to selected contour levels
xlabel('Time'); ylabel('x');
title('Temperature contour plot');
******************************************************************/

