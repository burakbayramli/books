// neutrn - Program to solve the neutron diffusion equation 
// using the Forward Time Centered Space (FTCS) scheme.
#include "NumMeth.h"
         
void main() {

  //* Initialize parameters (time step, grid spacing, etc.).
  cout << "Enter time step: "; double tau; cin >> tau;
  cout << "Enter the number of grid points: "; int N; cin >> N;
  cout << "Enter system length: "; double L; cin >> L;  
  // The system extends from x=-L/2 to x=L/2
  double h = L/(N-1);  // Grid size
  double D = 1.;       // Diffusion coefficient
  double C = 1.;       // Generation rate
  double coeff = D*tau/(h*h);
  double coeff2 = C*tau;
  if( coeff < 0.5 )
    cout << "Solution is expected to be stable" << endl;
  else
    cout << "WARNING: Solution is expected to be unstable" << endl;

  //* Set initial and boundary conditions.
  Matrix nn(N), nn_new(N);
  nn.set(0.0);     // Initialize density to zero at all points
  nn(N/2) = 1/h;   // Initial cond. is delta function in center
  //// The boundary conditions are nn(1) = nn(N) = 0
  nn_new.set(0.0);  // End points are unchanged during iteration

  //* Set up loop and plot variables.
  int iplot = 1;                 // Counter used to count plots
  cout << "Enter number of time steps: "; int nStep; cin >> nStep;
  int plot_step = 200;           // Number of time steps between plots
  int nplots = nStep/plot_step + 1;  // Number of snapshots (plots)
  Matrix xplot(N), tplot(nplots), nnplot(N,nplots), nAve(nplots);
  int i,j;
  for( i=1; i<=N; i++ )
    xplot(i) = (i-1)*h - L/2;   // Record the x scale for plots

  //* Loop over the desired number of time steps.
  int iStep;
  for( iStep=1; iStep<=nStep; iStep++ ) {

    //* Compute new density using FTCS scheme.
    for( i=2; i<=(N-1); i++ )
      nn_new(i) = nn(i) + coeff*(nn(i+1) + nn(i-1) - 2*nn(i))
	                    + coeff2*nn(i);
    
    nn = nn_new;     // Reset density to new values
  
    //* Periodically record density for plotting.
    if( (iStep%plot_step) < 1 ) { // Every plot_step steps ...
	  double nSum = 0;
      for( i=1; i<=N; i++ ) {      
        nnplot(i,iplot) = nn(i);  // Record tt(i) for plotting 
		nSum += nn(i);
	  }
	  nAve(iplot) = nSum/N;
      tplot(iplot) = iStep*tau;   // Record time for plots
      iplot++;
    }
  }
  nplots = iplot-1;  // Number of plots actually recorded
  
  //* Print out the plotting variables: tplot, xplot, nnplot, nAve
  ofstream tplotOut("tplot.txt"), xplotOut("xplot.txt"), 
	       nnplotOut("nnplot.txt"), nAveOut("nAve.txt");
  for( i=1; i<=nplots; i++ ) { 
    tplotOut << tplot(i) << endl;
	nAveOut << nAve(i) << endl;
  }
  for( i=1; i<=N; i++ ) {
    xplotOut << xplot(i) << endl;
    for( j=1; j<nplots; j++ )
      nnplotOut << nnplot(i,j) << ", ";
    nnplotOut << nnplot(i,nplots) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load tplot.txt; load xplot.txt; load nnplot.txt; load nAve.txt;
%* Plot density versus x and t as a 3D-surface plot
figure(1); clf;
surf(tplot,xplot,nnplot);
xlabel('Time');  ylabel('x');  zlabel('n(x,t)');
title('Neutron diffusion');
%* Plot average neutron density versus time
figure(2); clf;
plot(tplot,nAve,'*');
xlabel('Time'); ylabel('Average density');
******************************************************************/

