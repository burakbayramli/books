/* improved version by Mario Pernici (Mario.Pernici@mi.infn.it) */
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <math.h>

double myfunc(double y)
{
  if (y >= 0.0) {
    return pow(y,5.0)*exp(-y);
  } else {
    return 0.0;
  }
}

int main (int argc, char* argv[])
{
  char*  infilename;  char* outfilename;
  if (argc <= 2) {
    std::cout << "Usage: " << argv[0] << " infile outfile" << std::endl; 
    exit(1);
  } else {
    infilename = argv[1]; outfilename = argv[2];
  }
  std::ifstream ifile( infilename);
  std::ofstream ofile(outfilename);
  std::cout << argv[0] << ": converting " << infilename << " to "
	    << outfilename << std::endl;

  double x, y;
  std::ostringstream os;
  
  while (ifile >> x >> y) {
      y = myfunc(y);
      // printf's %g format
      os.unsetf(std::ios::floatfield);
      os << x << " ";
      os.setf(std::ios::scientific, std::ios::floatfield);
      os.precision(5);
      os << y << std::endl;
  }
  ofile << os.str();
}





