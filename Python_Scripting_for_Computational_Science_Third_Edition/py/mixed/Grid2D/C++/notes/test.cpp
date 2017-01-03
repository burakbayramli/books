// This program does not work properly

#include <NumPyArray.h>
#define PY_ARRAY_UNIQUE_SYMBOL mytest
#include <iostream>

extern "C" {
void test()
{
  npy_intp dim1[1]; dim1[0] = 3; 
  PyArrayObject* a = (PyArrayObject*) PyArray_FromDims(1, dim1, NPY_DOUBLE);
}
}

int main()
{
  std::cout << "H1" << std::endl;
  import_array();   /* required NumPy initialization */
  std::cout << "H1" << std::endl;
  test();
  std::cout << "H1" << std::endl;
  NumPyArray_Float x0;  x0.create(3);
  NumPyArray_Float x1(3);
  x1(0) = -1; x1(1) = 0; x1(2) = 5;
  dump(std::cout, x1);

  NumPyArray_Float x2(3,1);
  x2(0,0) = -1; x2(1,0) = 0; x2(2,0) = 5;
  dump(std::cout, x2);

  NumPyArray_Float x3(3,1,2);
  x3(0,0,0) = -1; x3(1,0,0) = 0; x3(2,0,0) = 5;
  x3(0,0,1) = -1; x3(1,0,1) = 0; x3(2,0,1) = 5;
  dump(std::cout, x3);
}
