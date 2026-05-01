// advect - Program to solve the advection equation 
// using the various hyperbolic PDE schemes
#include "NumMeth.h"
         
void main() {

  //* Select numerical parameters (time step, grid spacing, etc.).
  cout << "Choose a numerical method: 1) FTCS, 2) Lax, 3) Lax-Wendroff : ";
  int method; cin >> method;
  cout << "Enter number of grid points: "; int N; cin >> N;
  double L = 1.;     // System size
  double h = L/N;    // Grid spacing
  double c = 1;      // Wave speed
  cout << "Time for wave to move one grid spacing is " << h/c << endl;
  cout << "Enter time step: "; double tau; cin >> tau;
  double coeff = -c*tau/(2.*h);    // Coefficient used by all schemes
  double coefflw = 2*coeff*coeff;  // Coefficient used by L-W scheme
  cout << "Wave circles system in " << L/(c*tau) << " steps" << endl;
  cout << "Enter number of steps: "; int nStep; cin >> nStep;

  //* Set initial and boundary conditions.
  const double pi = 3.141592654;
  double sigma = 0.1;              // Width of the Gaussian pulse
  double k_wave = pi/sigma;        // Wave number of the cosine
  Matrix x(N), a(N), a_new(N);
  int i,j;
  for( i=1; i<=N; i++ ) {
    x(i) = (i-0.5)*h - L/2;  // Coordinates of grid points
    // Initial condition is a Gaussian-cosine pulse
    a(i) = cos(k_wave*x(i)) * exp(-x(i)*x(i)/(2*sigma*sigma)); 
  }
  // Use periodic boundary conditions
  int *ip, *im;  ip = new int [N+1];  im = new int [N+1];
  for( i=2; i<N; i++ ) {
    ip[i] = i+1;    // ip[i] = i+1 with periodic b.c.
    im[i] = i-1;    // im[i] = i-1 with periodic b.c.
  }
  ip[1] = 2;  ip[N] = 1;
  im[1] = N;  im[N] = N-1;
  
  //* Initialize plotting variables.
  int iplot = 1;          // Plot counter
  int nplots = 50;        // Desired number of plots
  double plotStep = ((double)nStep)/nplots; 
  Matrix aplot(N,nplots+1), tplot(nplots+1);
  tplot(1) = 0;       // Record the initial time (t=0)
  for( i=1; i<=N; i++ )
    aplot(i,1) = a(i);  // Record the initial state

  //* Loop over desired number of steps.
  int iStep;
  for( iStep=1; iStep<=nStep; iStep++ ) {  

    //* Compute new values of wave amplitude using FTCS, 
    //  Lax or Lax-Wendroff method.
    if( method == 1 )      ////// FTCS method //////
      for( i=1; i<=N; i++ )
        a_new(i) = a(i) + coeff*( a(ip[i])-a(im[i]) );  
    else if( method == 2 )  ////// Lax method //////
      for( i=1; i<=N; i++ )
        a_new(i) = 0.5*( a(ip[i])+a(im[i]) ) + 
                   coeff*( a(ip[i])-a(im[i]) );   
    else                   ////// Lax-Wendroff method //////
      for( i=1; i<=N; i++ )
        a_new(i) = a(i) + coeff*( a(ip[i])-a(im[i]) ) +
                   coefflw*( a(ip[i])+a(im[i])-2*a(i) ); 
     
    a = a_new;	 // Reset with new amplitude values

    //* Periodically record a(t) for plotting.
    if( fmod((double)iStep,plotStep) < 1 ) {  
      iplot++;
      tplot(iplot) = tau*iStep;
      for( i=1; i<=N; i++ )    
        aplot(i,iplot) = a(i);       // Record a(i) for ploting
      cout << iStep << " out of " << nStep << " steps completed" << endl;
    }
  }
  nplots = iplot;   // Actual number of plots recorded
  
  //* Print out the plotting variables: x, a, tplot, aplot
  ofstream xOut("x.txt"), aOut("a.txt"), 
	      tplotOut("tplot.txt"), aplotOut("aplot.txt");
  for( i=1; i<=N; i++ ) {
    xOut << x(i) << endl;
    aOut << a(i) << endl;
    for( j=1; j<nplots; j++ )
      aplotOut << aplot(i,j) << ", ";
    aplotOut << aplot(i,nplots) << endl;
  }
  for( i=1; i<=nplots; i++ )
    tplotOut << tplot(i) << endl;

  delete [] ip, im;  // Release allocated memory
}
/***** To plot in MATLAB; use the script below ********************
%* Plot the initial and final states.
load x.txt; load a.txt; load tplot.txt; load aplot.txt;
figure(1); clf;  % Clear figure 1 window and bring forward
plot(x,aplot(:,1),'-',x,a,'--');
legend('Initial','Final');
xlabel('x');  ylabel('a(x,t)');
pause(1);    % Pause 1 second between plots
%* Plot the wave amplitude versus position and time
figure(2); clf;  % Clear figure 2 window and bring forward
mesh(tplot,x,aplot);
ylabel('Position');  xlabel('Time'); zlabel('Amplitude');
view([-70 50]);  % Better view from this angle
******************************************************************/
