// relax - Program to solve the Laplace equation using 
// Jacobi, Gauss-Seidel and SOR methods on a square grid
#include "NumMeth.h"
         
void main() {

  //* Initialize parameters (system size, grid spacing, etc.)
  cout << "Select a numerical method: 1) Jacobi, 2) Gauss-Seidel, 3) SOR : ";
  int method; cin >> method;
  cout << "Enter number of grid points on a side: "; int N; cin >> N;
  double L = 1;          // System size (length)
  double h = L/(N-1);    // Grid spacing
  Matrix x(N), y(N);
  int i,j;
  for( i=1; i<=N; i++ ) 
    x(i) = (i-1)*h;  // x coordinate
  y = x;             // y coordinate

  //* Select over-relaxation factor (SOR only)
  double omega, omegaOpt, pi = 3.141592654;
  if( method == 3 ) {
    omegaOpt = 2.0/(1.0+sin(pi/N));  // Theoretical optimum
    cout << "Theoretical optimum omega = " << omegaOpt << endl;
    cout << "Enter desired omega: "; cin >> omega;
  }

  //* Set initial guess as first term in separation of variables soln.
  double phi0 = 1;     // Potential at y=L
  double coeff = phi0 * 4/(pi*sinh(pi));
  Matrix phi(N,N);
  for( i=1; i<=N; i++ )
    for( j=1; j<=N; j++ )
      phi(i,j) = coeff * sin(pi*x(i)/L) * sinh(pi*y(j)/L); 

  //* Set boundary conditions
  for( i=1; i<=N; i++ ) {
    phi(i,1) = 0.0;  
    phi(i,N) = phi0;
  }
  for( j=1; j<=N; j++ ) {
    phi(1,j) = 0.0;  
    phi(N,j) = 0.0;
  }
  cout << "Potential at y=L equals " << phi0 << endl;
  cout << "Potential is zero on all other boundaries" << endl;

  //* Loop until desired fractional change per iteration is obtained
  Matrix newphi(N,N);     // Copy of the solution (used only by Jacobi)
  newphi = phi;
  double phiTemp;         // Temporary value used by GS and SOR
  int iterMax = N*N;            // Set max to avoid excessively long runs
  double changeDesired = 1e-4;  // Stop when the change is given fraction
  cout << "Desired fractional change = " << changeDesired << endl;
  Matrix change(iterMax); // Record fractional change at each iteration
  int iter, nIter;        // Iterations counters
  for( iter=1; iter<=iterMax; iter++ ) {
  
    double changeSum = 0;
    if( method == 1 )  {    //// Jacobi method ////
      for( i=2; i<=(N-1); i++ )  // Loop over interior points only
       for( j=2; j<=(N-1); j++ ) {    
         newphi(i,j) = 0.25*(phi(i+1,j)+phi(i-1,j)+ 
                               phi(i,j-1)+phi(i,j+1));
         changeSum += fabs(1-phi(i,j)/newphi(i,j));
       }
      phi = newphi;   // Copy new values into phi                 
	}
    else if( method == 2 )  //// G-S method ////     
      for( i=2; i<=(N-1); i++ )  // Loop over interior points only
       for( j=2; j<=(N-1); j++ ) {    
         phiTemp = 0.25*(phi(i+1,j)+phi(i-1,j)+ 
                                phi(i,j-1)+phi(i,j+1));
         changeSum += fabs(1-phi(i,j)/phiTemp);
         phi(i,j) = phiTemp;
       } 
	else                   //// SOR method ////    
      for( i=2; i<=(N-1); i++ )  // Loop over interior points only
       for( j=2; j<=(N-1); j++ ) {    
         phiTemp = 0.25*omega*(phi(i+1,j)+phi(i-1,j)+ 
                phi(i,j-1)+phi(i,j+1))  +  (1-omega)*phi(i,j);
         changeSum += fabs(1-phi(i,j)/phiTemp);
         phi(i,j) = phiTemp;
       }
	
    //* Check if fractional change is small enough to halt the iteration
    change(iter) = changeSum/((N-2)*(N-2));
    if( (iter%10) < 1 )
      cout << "After " << iter << " iterations, fractional change = "
                     << change(iter) << endl;                            						
    if( change(iter) < changeDesired ) {
      cout << "Desired accuracy achieved after " << iter 
           << " iterations" << endl; 
	  cout << "Breaking out of main loop" << endl;
      nIter = iter;
      break;	    // Break out of the main loop
    }
  }

  //* Print out the plotting variables: x, y, phi, change
  ofstream xOut("x.txt"), yOut("y.txt"), 
	       phiOut("phi.txt"), changeOut("change.txt");
  for( i=1; i<=N; i++ ) {
    xOut << x(i) << endl;
    yOut << y(i) << endl;
    for( j=1; j<N; j++ )
      phiOut << phi(i,j) << ", ";
    phiOut << phi(i,N) << endl;
  }
  for( i=1; i<=nIter; i++ ) 
    changeOut << change(i) << endl;  
}
/***** To plot in MATLAB; use the script below ********************
load x.txt; load y.txt; load phi.txt; load change.txt;
%* Plot final estimate of potential as contour and surface plots
figure(1); clf;
cLevels = 0:(0.1):1;    % Contour levels
cs = contour(x,y,flipud(rot90(phi)),cLevels); 
xlabel('x'); ylabel('y'); clabel(cs);
figure(2); clf;
mesh(x,y,flipud(rot90(phi)));
xlabel('x'); ylabel('y'); zlabel('\Phi(x,y)');
%* Plot the fractional change versus iteration
figure(3); clf;
semilogy(change);
xlabel('Iteration');  ylabel('Fractional change');
******************************************************************/
