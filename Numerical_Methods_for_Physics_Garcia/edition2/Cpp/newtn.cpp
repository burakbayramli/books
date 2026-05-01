// newtn - Program to solve a system of nonlinear equations 
// using Newton's method.  Equations defined by function fnewt.
#include "NumMeth.h"

void ge(Matrix a, Matrix b, Matrix& x);
void fnewt(Matrix x, Matrix a, Matrix& f, Matrix& D);
         
void main() {

  //* Set initial guess and parameters
  int iStep, nStep = 10;   // Number of iterations before stopping
  int nVars = 3, nParams = 3;  // Number of variables and parameters
  Matrix x(nVars), xp(nVars,nStep+1);
  cout << "Enter the initial guess: " << endl;
  int i,j;
  for( i=1; i<=nVars; i++ ) {
    cout << " x(" << i << ") = "; cin >> x(i);
    xp(i,1) = x(i); // Record initial guess for plotting
  }
  Matrix a(nParams);
  cout << "Enter the parameters: " << endl;
  for( i=1; i<=nParams; i++ ) {
    cout << "a(" << i << ") = "; cin >> a(i);
  }

  //* Loop over desired number of steps 
  Matrix f(nVars), D(nVars,nVars), dx(nVars);
  for( iStep=1; iStep<=nStep; iStep++ )	 {
	
    //* Evaluate function f and its Jacobian matrix D
    fnewt(x,a,f,D);      // fnewt returns value of f and D
	for( i=1; i<=nVars; i++ )
	  for( j=i+1; j<=nVars; j++ )  {
		double temp = D(i,j);
	    D(i,j) = D(j,i);	  // Transpose of matrix D
		D(j,i) = temp;
	  }
    
    //* Find dx by Gaussian elimination
    ge(D,f,dx); 
    
    //* Update the estimate for the root 
    for( i=1; i<=nVars; i++ ) { 
      x(i) -= dx(i);            // Newton iteration for new x
      xp(i,iStep+1) = x(i);  // Save current estimate for plotting
    }
  }

  //* Print the final estimate for the root
  cout << "After " << nStep << " iterations the root is:" << endl;
  for( i=1; i<=nVars; i++ )
    cout << "x(" << i << ") = " << x(i) << endl;
    
  //* Print out the plotting variable: xp
  ofstream xpOut("xp.txt");
  for( i=1; i<=nVars; i++ ) {
    for( int j=1; j<=nStep; j++ )
      xpOut << xp(i,j) << ", ";
    xpOut << xp(i,nStep+1) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load xp.txt;
%* Plot the iterations from initial guess to final estimate
figure(1); clf;  % Clear figure 1 window and bring forward
subplot(1,2,1) % Left plot
  plot3(xp(1,:),xp(2,:),xp(3,:),'o-');
  xlabel('x');  ylabel('y'); zlabel('z');
  view([-37.5, 30]);  % Viewing angle
  grid; drawnow;
subplot(1,2,2) % Right plot
  plot3(xp(1,:),xp(2,:),xp(3,:),'o-');
  xlabel('x');  ylabel('y'); zlabel('z');
  view([-127.5, 30]);  % Viewing angle
  grid; drawnow;
% Plot data from lorenz (if available).
flag = input('Plot data from lorenz program? (1=Yes/0=No): ');
if( flag == 1 )
  figure(2); clf;  % Clear figure 1 window and bring forward
  load xplot.txt; load yplot.txt; load zplot.txt;
  plot3(xplot,yplot,zplot,'-',xp(1,:),xp(2,:),xp(3,:),'o--');
  xlabel('x'); ylabel('y'); zlabel('z');
  view([40 10]);  % Rotate to get a better view 
  grid;           % Add a grid to aid perspective
end
******************************************************************/
