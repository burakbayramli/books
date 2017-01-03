#include <stdlib.h>
#include <iostream>
#include <math.h>

double hw1(double r1, double r2)
{
  double s = sin(r1 + r2);
  return s;
}

void hw2(double r1, double r2)
{
  double s = sin(r1 + r2);
  std::cout << "Hello, World! sin(" << r1 << "+" << r2 
	    << ")=" << s << std::endl;
}

void hw3(double r1, double r2, double* s)
{
  *s = sin(r1 + r2);
}

void hw4(double r1, double r2, double& s)
{
  s = sin(r1 + r2);
}

int main(int argc, const char* argv[])
{
  if (argc < 3) {  /* need two command-line arguments */
    std::cout << "Usage: " << argv[0] << " r1 r2\n";  exit(1);
  }
  double r1 = atof(argv[1]);  double r2 = atof(argv[2]);
  std::cout << "hw1, result: " << hw1(r1, r2) << std::endl;
  std::cout << "hw2, result: ";
  hw2(r1, r2);
  std::cout << "hw3, result: ";
  double s; hw3(r1, r2, &s);
  std:: cout << s << std::endl;
  std::cout << "hw4, result: ";
  hw4(r1, r2, s);
  std:: cout << s << std::endl;
}
 
	
