#include <iostream>
#include <fstream>
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
  // abort if there are too few command-line arguments
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
  int ok = 1;  // boolean variable for not end of file
  while (ok) {
    if (!(ifile >> x >> y)) ok = 0;
    if (ok) {
      //std::cout.setf(0 /*printf's %g format*/, std::ios::floatfield);
      //std::cout << x;
      //std::cout.setf(std::ios::scientific, std::ios::floatfield);
      //std::cout.precision(5);
      //std::cout.width(12);
      //std::cout << " " << y << std::endl;

      y = myfunc(y);
      ofile.unsetf(std::ios::floatfield);
      ofile << x << " ";
      ofile.setf(std::ios::scientific, std::ios::floatfield);
      ofile.precision(5);
      //ofile.width(12); 
      ofile << y << std::endl;
    }
  }
  ifile.close();  ofile.close();
  return 0;
}





