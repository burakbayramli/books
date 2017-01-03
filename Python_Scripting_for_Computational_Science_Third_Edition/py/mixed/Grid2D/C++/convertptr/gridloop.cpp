#include <MyArray.h>
#include <gridloop.h>

void gridloop1(MyArray<double>& a, 
	       const MyArray<double>& xcoor,
	       const MyArray<double>& ycoor,
	       Fxy func1)
{
  int nx = a.shape(1), ny = a.shape(2);
  int i, j;
  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
      a(i,j) = func1(xcoor(i), ycoor(j));
    }
  }
}

MyArray<double>& gridloop2(const MyArray<double>& xcoor,
			   const MyArray<double>& ycoor,
			   Fxy func1)
{
  int nx = xcoor.shape(1), ny = ycoor.shape(1);
  MyArray<double>& a = *new MyArray<double>(nx, ny);
  int i, j;
  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
      a(i,j) = func1(xcoor(i), ycoor(j));
    }
  }
  return a;
}

