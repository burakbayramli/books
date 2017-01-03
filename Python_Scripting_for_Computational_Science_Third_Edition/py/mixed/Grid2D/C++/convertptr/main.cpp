#include <MyArray.h>
#include <math.h>
#include <stdlib.h>
#include "gridloop.h"

double myfunc(double x, double y)
{
  return sin(x*y) + 8*x;
}

int main(int argc, const char* argv[])
{
  int N;
  if (argc == 2) {
    N = atoi(argv[1]);
  } else {
    std::cout << "Usage: tmp.app N" << std::endl;
    exit(1);
  }

  int i, j;
  const int repetitions=50;

  if (N > 9) {
    MyArray<double> A(N,N);
    MyArray<double> xcoor(N), ycoor(N);
    double dx = 1.0/N;
    for (i = 0; i < N; i++) {
      xcoor(i) = i*dx;  ycoor(i) = i*dx;
    }
    for (int counter = 1; counter <= repetitions; counter++) {
      gridloop1(A, xcoor, ycoor, myfunc);
    }
    std::cout << "gridloop1 called " << repetitions << " times!" << std::endl;
  }
  else {
    // verification:
    MyArray<double> a(N);
    for (i = 0; i < N; i++) {
      a(i) = i+1;
    }
    std::cout << a;
    MyArray<double> b(N,N-1);
    for (i = 0; i < N; i++) {
      for (j = 0; j < N-1; j++) {
	b(i,j) = i+j-2;
      }
    }
    std::cout << b;
  }
}



  
