#ifndef GRIDLOOP_H
#define GRIDLOOP_H
#include <MyArray.h>

typedef double (*Fxy)(double x, double y);

void gridloop1(MyArray<double>& a, 
	       const MyArray<double>& xcoor,
	       const MyArray<double>& ycoor,
	       Fxy func1);

MyArray<double>& gridloop2(
               const MyArray<double>& xcoor,
	       const MyArray<double>& ycoor,
	       Fxy func1);
#endif
