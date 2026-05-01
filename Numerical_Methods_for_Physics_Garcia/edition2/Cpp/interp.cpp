// interp - Program to interpolate data using Lagrange 
// polynomial to fit quadratic to three data points
#include "NumMeth.h"

double intrpf( double xi, double x[], double y[]);

void main() {

  //* Initialize the data points to be fit by quadratic
  double x[3+1], y[3+1];
  cout << "Enter data points:" << endl;
  int i;
  for( i=1; i<=3; i++ ) {
    cout << "x[" << i << "] = ";
    cin >> x[i];
    cout << "y[" << i << "] = ";
    cin >> y[i];
  }

  //* Establish the range of interpolation (from x_min to x_max)
  double x_min, x_max;
  cout << "Enter minimum value of x: ";  cin >> x_min;
  cout << "Enter maximum value of x: ";  cin >> x_max;

  //* Find yi for the desired interpolation values xi using
  //  the function intrpf
  int nplot = 100;     // Number of points for interpolation curve
  double *xi, *yi;
  xi = new double [nplot+1];   // Allocate memory for these
  yi = new double [nplot+1];   // arrays (nplot+1 elements)
  for( i=1; i<=nplot; i++ ) {
    xi[i] = x_min + (x_max-x_min)*(i-1)/(nplot-1);
    yi[i] = intrpf(xi[i],x,y);  // Use intrpf function to interpolate
  }
  
  //* Print out the plotting variables: x, y, xi, yi
  ofstream xOut("x.txt"), yOut("y.txt"), xiOut("xi.txt"), 
	       yiOut("yi.txt");
  for( i=1; i<=3; i++ ) {
    xOut << x[i] << endl;
    yOut << y[i] << endl;
  }
  for( i=1; i<=nplot; i++ ) {
    xiOut << xi[i] << endl;
    yiOut << yi[i] << endl;
  }

  delete [] xi, yi;	 // Release memory allocated by "new"
}
/***** To plot in MATLAB; use the script below ********************
load x.txt; load y.txt; load xi.txt; load yi.txt;
plot(x,y,'*',xi,yi,'-');
xlabel('x');  ylabel('y');
title('Three point interpolation');
legend('Data points','Interpolation');
******************************************************************/
