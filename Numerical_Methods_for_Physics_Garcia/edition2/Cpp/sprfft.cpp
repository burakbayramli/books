// sprfft - Program to compute the power spectrum of a  
// coupled mass-spring system.
#include "NumMeth.h"

void sprrk(double x[], double t, double param[], double deriv[]);
void rk4( double x[], int nX, double t, double tau, 
  void (*derivsRK)(double x[], double t, double param[], double deriv[]), 
  double param[]);
void fft( Matrix& RealA, Matrix& ImagA);
         
void main() {

  //* Set parameters for the system (initial positions, etc.).
  const int nState = 6;
  Matrix x(3), v(3); double state[nState+1];
  cout << "Enter initial displacement of:" << endl;
  cout << "  Mass #1 = "; cin >> x(1);
  cout << "  Mass #2 = "; cin >> x(2);
  cout << "  Mass #3 = "; cin >> x(3);
  v(1) = 0.0; v(2) = 0.0; v(3) = 0.0; // Masses are initially at rest
  state[1] = x(1);  state[2] = x(2);  state[3] = x(3);
  state[4] = v(1);  state[5] = v(2);  state[6] = v(3);
  cout << "Enter timestep: "; double tau; cin >> tau;  
  double k_over_m = 1;      // Ratio of spring const. over mass
  double param[1+1]; param[1] = k_over_m;

  //* Loop over the desired number of time steps.
  double time = 0;       // Set initial time
  int nStep = 256;       // Number of steps in the main loop
  int nprint = nStep/8;  // Number of steps between printing progress
  Matrix xplot(nStep,3), tplot(nStep);  // Plotting variables
  int i, iStep;
  for( iStep=1; iStep<=nStep; iStep++ ) { 

    //* Use Runge-Kutta to find new displacements of the masses.
    rk4(state,nState,time,tau,sprrk,param);  
    time = time + tau;    
  
    //* Record the positions for graphing and to compute spectra.
    xplot(iStep,1) = state[1];   // Record positions
    xplot(iStep,2) = state[2];  xplot(iStep,3) = state[3];
    tplot(iStep) = time;
    if( (iStep%nprint) < 1 )
      cout << "Finished " << iStep << " out of " << 
                             nStep << " steps" << endl;
  }

  //* Calculate the power spectrum of the time series for mass #1
  Matrix f(nStep), x1fftR(nStep), x1fftI(nStep), spect(nStep);
  for( i=1; i<=nStep; i++ ) {
    f(i) = (i-1)/(tau*nStep);      // Frequency
    double x1 = xplot(i,1);        // Displacement of mass 1
    x1fftR(i) = x1;
    x1fftI(i) = 0.0;     // Copy data for input to fft
  }
  fft(x1fftR, x1fftI);         // Fourier transform of displacement
  for( i=1; i<=nStep; i++ )    // Power spectrum of displacement
    spect(i) = x1fftR(i)*x1fftR(i) + x1fftI(i)*x1fftI(i);         

  //* Apply the Hanning window to the time series and calculate
  //  the resulting power spectrum
  double window, pi = 3.141592654;
  Matrix x1fftRw(nStep), x1fftIw(nStep), spectw(nStep);
  for( i=1; i<=nStep; i++ ) {
    window = 0.5*(1.0-cos(2.0*pi*(i-1.0)/nStep)); // Hanning window
    double x1w = xplot(i,1) * window;      // Windowed time series
    x1fftRw(i) = x1w;
    x1fftIw(i) = 0.0;     // Copy data for input to fft
  }
  fft(x1fftRw, x1fftIw);            // Fourier transf. (windowed data)
  for( i=1; i<=nStep; i++ )    // Power spectrum (windowed data)
    spectw(i) = x1fftRw(i)*x1fftRw(i) + x1fftIw(i)*x1fftIw(i);         
      
  //* Print out the plotting variables: 
  //    tplot, xplot, f, spect, spectw
  ofstream tplotOut("tplot.txt"), xplotOut("xplot.txt"), fOut("f.txt"), 
	       spectOut("spect.txt"), spectwOut("spectw.txt");
  for( i=1; i<=nStep; i++ ) {
    tplotOut << tplot(i) << endl;
    xplotOut << xplot(i,1) << ", " << xplot(i,2) << ", "
             << xplot(i,3) << endl;
    fOut << f(i) << endl;
    spectOut << spect(i) << endl;
    spectwOut << spectw(i) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load tplot.txt; load xplot.txt; load f.txt; 
load spect.txt; load spectw.txt
nstep = length(tplot);  nprint = nstep/8;
%* Graph the displacements of the three masses.
figure(1); clf;  % Clear figure 1 window and bring forward
ipr = 1:nprint:nstep;  % Used to graph limited number of symbols
plot(tplot(ipr),xplot(ipr,1),'o',tplot(ipr),xplot(ipr,2),'+',...
     tplot(ipr),xplot(ipr,3),'*',...
     tplot,xplot(:,1),'-',tplot,xplot(:,2),'-.',...
     tplot,xplot(:,3),'--');
legend('Mass #1','Mass #2','Mass #3');
title('Displacement of masses (relative to rest positions)');
xlabel('Time'); ylabel('Displacement');
%* Graph the power spectra for original and windowed data
figure(2); clf;  % Clear figure 2 window and bring forward
semilogy(f(1:(nstep/2)),spect(1:(nstep/2)),'-',...
         f(1:(nstep/2)),spectw(1:(nstep/2)),'--');
title('Power spectrum (dashed is windowed data)');
xlabel('Frequency'); ylabel('Power');
******************************************************************/
