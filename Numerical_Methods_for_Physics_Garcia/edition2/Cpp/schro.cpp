//  schro - Program to solve the Schrodinger equation 
//  for a free particle using the Crank-Nicolson scheme
#include "NumMeth.h"
         
void cinv( Matrix RealA, Matrix ImagA, 
			 Matrix& RealAinv, Matrix& ImagAinv );
 
void main() {

  //* Initialize parameters (grid spacing, time step, etc.)
  cout << "Enter number of grid points: "; int N; cin >> N;
  double L = 100;        // System extends from -L/2 to L/2
  double h = L/(N-1);    // Grid size
  double h_bar = 1;  double mass = 1; // Natural units
  cout << "Enter time step: "; double tau; cin >> tau;
  Matrix x(N);
  int i, j, k;
  for( i=1; i<=N; i++ )
    x(i) = h*(i-1) - L/2;  // Coordinates  of grid points

  //* Set up the Hamiltonian operator matrix
  Matrix eye(N,N), ham(N,N);
  eye.set(0.0);  // Set all elements to zero
  for( i=1; i<=N; i++ ) // Identity matrix
    eye(i,i) = 1.0;
  ham.set(0.0);  // Set all elements to zero
  double coeff = -h_bar*h_bar/(2*mass*h*h);
  for( i=2; i<=(N-1); i++ ) {
    ham(i,i-1) = coeff;
    ham(i,i) = -2*coeff;  // Set interior rows
    ham(i,i+1) = coeff;
  }
  // First and last rows for periodic boundary conditions
  ham(1,N) = coeff;   ham(1,1) = -2*coeff; ham(1,2) = coeff;
  ham(N,N-1) = coeff; ham(N,N) = -2*coeff; ham(N,1) = coeff;

  //* Compute the Crank-Nicolson matrix
  Matrix RealA(N,N), ImagA(N,N), RealB(N,N), ImagB(N,N);
  for( i=1; i<=N; i++ )
   for( j=1; j<=N; j++ ) {
     RealA(i,j) = eye(i,j);
     ImagA(i,j) = 0.5*tau/h_bar*ham(i,j);
     RealB(i,j) = eye(i,j);
     ImagB(i,j) = -0.5*tau/h_bar*ham(i,j);
   }
  Matrix RealAi(N,N), ImagAi(N,N);
  cout << "Computing matrix inverse ... " << flush;
  cinv( RealA, ImagA, RealAi, ImagAi );  // Complex matrix inverse
  cout << "done" << endl;
  Matrix RealD(N,N), ImagD(N,N); // Crank-Nicolson matrix
  for( i=1; i<=N; i++ )
   for( j=1; j<=N; j++ ) {
    RealD(i,j) = 0.0;           // Matrix (complex) multiplication
    ImagD(i,j) = 0.0;
    for( k=1; k<=N; k++ ) {
      RealD(i,j) += RealAi(i,k)*RealB(k,j) - ImagAi(i,k)*ImagB(k,j);
      ImagD(i,j) += RealAi(i,k)*ImagB(k,j) + ImagAi(i,k)*RealB(k,j);
    }
   }
			 
  //* Initialize the wavefunction 
  const double pi = 3.141592654;
  double x0 = 0;          // Location of the center of the wavepacket
  double velocity = 0.5;  // Average velocity of the packet
  double k0 = mass*velocity/h_bar;       // Average wavenumber
  double sigma0 = L/10;   // Standard deviation of the wavefunction
  double Norm_psi = 1/(sqrt(sigma0*sqrt(pi)));  // Normalization
  Matrix RealPsi(N), ImagPsi(N), rpi(N), ipi(N);
  for( i=1; i<=N; i++ ) {
    double expFactor = exp(-(x(i)-x0)*(x(i)-x0)/(2*sigma0*sigma0));
    RealPsi(i) = Norm_psi * cos(k0*x(i)) * expFactor;
    ImagPsi(i) = Norm_psi * sin(k0*x(i)) * expFactor;
    rpi(i) = RealPsi(i);    // Record initial wavefunction
    ipi(i) = ImagPsi(i);    // for plotting
  }

  //* Initialize loop and plot variables 
  int nStep = (int)(L/(velocity*tau));  // Particle should circle system
  int nplots = 20;                      // Number of plots to record
  double plotStep = nStep/nplots;       // Iterations between plots          
  Matrix p_plot(N,nplots+2);
  for( i=1; i<=N; i++ )      // Record initial condition
    p_plot(i,1) = RealPsi(i)*RealPsi(i) + ImagPsi(i)*ImagPsi(i);
  int iplot = 1;

  //* Loop over desired number of steps (wave circles system once)
  int iStep;
  Matrix RealNewPsi(N), ImagNewPsi(N);
  for( iStep=1; iStep<=nStep; iStep++ )	{
	
    //* Compute new wave function using the Crank-Nicolson scheme
	RealNewPsi.set(0.0);  ImagNewPsi.set(0.0);
    for( i=1; i<=N; i++ )        // Matrix multiply D*psi
      for( j=1; j<=N; j++ ) {
        RealNewPsi(i) += RealD(i,j)*RealPsi(j) - ImagD(i,j)*ImagPsi(j);
        ImagNewPsi(i) += RealD(i,j)*ImagPsi(j) + ImagD(i,j)*RealPsi(j);
      }
    RealPsi = RealNewPsi;  // Copy new values into Psi
    ImagPsi = ImagNewPsi;
        
    //* Periodically record values for plotting
    if( fmod(iStep,plotStep) < 1 ) {  
      iplot++;
      for( i=1; i<=N; i++ ) 
        p_plot(i,iplot) = RealPsi(i)*RealPsi(i) + ImagPsi(i)*ImagPsi(i);
      cout << "Finished " << iStep << " of " << nStep << " steps" << endl; 
    }
  }
  // Record final probability density
  iplot++;
  for( i=1; i<=N; i++ ) 
    p_plot(i,iplot) = RealPsi(i)*RealPsi(i) + ImagPsi(i)*ImagPsi(i); 
  nplots = iplot;   // Actual number of plots recorded
  
  //* Print out the plotting variables:  x, rpi, ipi, p_plot
  ofstream xOut("x.txt"), rpiOut("rpi.txt"), ipiOut("ipi.txt"), 
	       p_plotOut("p_plot.txt");
  for( i=1; i<=N; i++ ) {
    xOut << x(i) << endl;
    rpiOut << rpi(i) << endl;
    ipiOut << ipi(i) << endl;
    for( j=1; j<nplots; j++ )
      p_plotOut << p_plot(i,j) << ", ";
    p_plotOut << p_plot(i,nplots) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load x.txt; load rpi.txt; load ipi.txt; load p_plot.txt;
%* Plot the initial wavefunction
figure(1); clf;
plot(x,rpi,x,ipi);
title('Initial wave function');
xlabel('x');  ylabel('\psi(x)'); legend('Real','Imag');
%* Plot probability versus position at various times
figure(2); clf;
[mp np] = size(p_plot);
plot(x,p_plot(:,1:3:np),x,p_plot(:,np));
xlabel('x'); ylabel('P(x,t)');
title('Probability density at various times');
axisV = [-1/2 1/2 0 max(p_plot)]; % Fix axis min and max
******************************************************************/
   
