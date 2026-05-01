// lsfdemo - Program for demonstrating least squares fit routines
#include "NumMeth.h"

void linreg( Matrix x, Matrix y, Matrix sigma,
            Matrix &a_fit, Matrix &sig_a, Matrix &yy, double &chisqr );
void pollsf( Matrix x, Matrix y, Matrix sigma, int M, 
             Matrix& a_fit, Matrix& sig_a, Matrix& yy, double& chisqr );
double randn( long& iseed );
         
void main() {

  //* Initialize data to be fit. Data is quadratic plus random number.
  Matrix c(3);
  cout << "Curve fit data is created using the quadratic" << endl;
  cout << "  y(x) = c(1) + c(2)*x + c(3)*x^2" << endl;
  cout << "Enter the coefficients:" << endl;
  cout << "c(1) = "; cin >> c(1);
  cout << "c(2) = "; cin >> c(2);
  cout << "c(3) = "; cin >> c(3);  
  double alpha;
  cout << "Enter estimated error bar: "; cin >> alpha;
  int i, N = 50;        // Number of data points
  long seed = 1234;     // Seed for random number generator
  Matrix x(N), y(N), sigma(N);
  for( i=1; i<=N; i++ ) {
    x(i) = i;                  // x = [1, 2, ..., N]
    y(i) = c(1) + c(2)*x(i) + c(3)*x(i)*x(i) + alpha*randn(seed);
    sigma(i) = alpha;          // Constant error bar
  }
  
  //* Fit the data to a straight line or a more general polynomial
  cout << "Enter number of fit parameters (=2 for line): ";
  int M;  cin >> M;
  Matrix a_fit(M), sig_a(M), yy(N);	 double chisqr;
  if( M == 2 )  //* Linear regression (Straight line) fit   
    linreg( x, y, sigma, a_fit, sig_a, yy, chisqr);
  else          //* Polynomial fit
    pollsf( x, y, sigma, M, a_fit, sig_a, yy, chisqr);

  //* Print out the fit parameters, including their error bars.
  cout << "Fit parameters:" << endl;
  for( i=1; i<=M; i++ )
    cout << " a(" << i << ") = " << a_fit(i) << 
            " +/- " << sig_a(i) << endl;

  cout << "Chi square = " << chisqr << "; N-M = " << N-M << endl;
  
  //* Print out the plotting variables: x, y, sigma, yy
  ofstream xOut("x.txt"), yOut("y.txt"), 
	       sigmaOut("sigma.txt"), yyOut("yy.txt");
  for( i=1; i<=N; i++ ) {
    xOut << x(i) << endl;
    yOut << y(i) << endl;
    sigmaOut << sigma(i) << endl;
    yyOut << yy(i) << endl;
  }
}
/***** To plot in MATLAB; use the script below ********************
load x.txt; load y.txt; load sigma.txt; load yy.txt
%* Graph the data, with error bars, and fitting function.
figure(1); clf;           % Bring figure 1 window forward
errorbar(x,y,sigma,'o');  % Graph data with error bars
hold on;                  % Freeze the plot to add the fit
plot(x,yy,'-');           % Plot the fit on same graph as data
xlabel('x_i'); ylabel('y_i and Y(x)');
******************************************************************/
