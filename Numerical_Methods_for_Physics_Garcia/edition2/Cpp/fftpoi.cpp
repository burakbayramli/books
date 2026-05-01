// fftpoi - Program to solve the Poisson equation using
// MFT method (periodic boundary conditions)
#include "NumMeth.h"

void fft2( Matrix& RealA, Matrix& ImagA);
void ifft2( Matrix& RealA, Matrix& ImagA);

void main() {

  //* Initialize parameters (system size, grid spacing, etc.)
  double eps0 = 8.8542e-12;   // Permittivity (C^2/(N m^2))
  int N = 64;   // Number of grid points on a side (square grid)
  double L = 1;    // System size
  double h = L/N;  // Grid spacing for periodic boundary conditions
  Matrix x(N), y(N);
  int i,j;
  for( i=1; i<=N; i++ )
    x(i) = (i-0.5)*h;  // Coordinates of grid points
  y = x;               // Square grid
  cout << "System is a square of length " << L << endl;

  //* Set up charge density rho(i,j)
  Matrix rho(N,N);
  rho.set(0.0);     // Initialize charge density to zero
  cout << "Enter number of line charges: "; int M; cin >> M;
  for( i=1; i<=M; i++ ) {
    cout << "For charge #" << i << endl;
    cout << "Enter x coordinate: "; double xc; cin >> xc;
    cout << "Enter y coordinate: "; double yc; cin >> yc;
    int ii = (int)(xc/h) + 1;    // Place charge at nearest
    int jj = (int)(yc/h) + 1;    // grid point
    cout << "Enter charge density: "; double q; cin >> q;
    rho(ii,jj) += q/(h*h);
  }

  //* Compute matrix P
  const double pi = 3.141592654;
  Matrix cx(N), cy(N);
  for( i=1; i<=N; i++ )
    cx(i) = cos((2*pi/N)*(i-1));
  cy = cx;
  Matrix RealP(N,N), ImagP(N,N);
  double numerator = -h*h/(2*eps0);
  double tinyNumber = 1e-20;  // Avoids division by zero
  for( i=1; i<=N; i++ )
   for( j=1; j<=N; j++ )
     RealP(i,j) = numerator/(cx(i)+cy(j)-2+tinyNumber);
  ImagP.set(0.0);

  //* Compute potential using MFT method
  Matrix RealR(N,N), ImagR(N,N), RealF(N,N), ImagF(N,N);
  for( i=1; i<=N; i++ )
   for( j=1; j<=N; j++ ) {
     RealR(i,j) = rho(i,j);
     ImagR(i,j) = 0.0;       // Copy rho into R for input to fft2
   }
  fft2(RealR,ImagR);   // Transform rho into wavenumber domain
  // Compute phi in the wavenumber domain
  for( i=1; i<=N; i++ )
   for( j=1; j<=N; j++ ) {
    RealF(i,j) = RealR(i,j)*RealP(i,j) - ImagR(i,j)*ImagP(i,j);
    ImagF(i,j) = RealR(i,j)*ImagP(i,j) + ImagR(i,j)*RealP(i,j);
   }
  Matrix phi(N,N);
  ifft2(RealF,ImagF);    // Inv. transf. phi into the coord. domain
  for( i=1; i<=N; i++ )
   for( j=1; j<=N; j++ )
     phi(i,j) = RealF(i,j);

  //* Print out the plotting variables: x, y, phi
  ofstream xOut("x.txt"), yOut("y.txt"), phiOut("phi.txt");
  for( i=1; i<=N; i++ ) {
    xOut << x(i) << endl;
    yOut << y(i) << endl;
    for( j=1; j<N; j++ )
      phiOut << phi(i,j) << ", ";
    phiOut << phi(i,N) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load x.txt; load y.txt; load phi.txt;
%* Compute electric field as E = - grad phi
[Ex Ey] = gradient(flipud(rot90(phi)));
magnitude = sqrt(Ex.^2 + Ey.^2);
Ex = -Ex ./ magnitude;     % Normalize components so
Ey = -Ey ./ magnitude;     % vectors have equal length
%* Plot potential and electric field
figure(1); clf;
contour3(x,y,flipud(rot90(phi,1)),35);
xlabel('x'); ylabel('y'); zlabel('\Phi(x,y)');
figure(2); clf;
quiver(x,y,Ex,Ey)        % Plot E field with vectors
title('E field (Direction)'); xlabel('x'); ylabel('y');
axis('square');  axis([0 1 0 1]);
******************************************************************/
