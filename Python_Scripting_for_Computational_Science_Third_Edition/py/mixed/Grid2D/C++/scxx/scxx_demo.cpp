#include <Python.h>        // Python C API
#include <PWONumber.h>     // class for numbers
#include <PWOSequence.h>   // class for tuples
#include <PWOMSequence.h>  // class for lists (immutable sequences)
#include <iostream>

void test_scxx()
{
  double a_ = 3.4;
  PWONumber a = a_; PWONumber b = 7;
  PWONumber c; c = a + b;
  PWOList list; list.append(a).append(c).append(b);
  PWOTuple tp(list);
  for (int i=0; i<tp.len(); i++) {
    std::cout << "tp[" << i << "]=" << double(PWONumber(tp[i])) << " ";
  }
  std::cout << std::endl;
  PyObject* py_a = (PyObject*) a;  // convert to Python C struct
}

void test_PythonAPI()
{
  double a_ = 3.4;
  PyObject* a = PyFloat_FromDouble(a_);
  PyObject* b = PyFloat_FromDouble(7);
  PyObject* c = PyNumber_Add(a, b); 
  PyObject* list = PyList_New(0);
  PyList_Append(list, a);
  PyList_Append(list, c);
  PyList_Append(list, b);
  PyObject* tp = PyList_AsTuple(list);
  int tp_len = PySequence_Length(tp);
  for (int i=0; i<tp_len; i++) {
    PyObject* qp = PySequence_GetItem(tp, i);
    double q = PyFloat_AS_DOUBLE(qp);
    std::cout << "tp[" << i << "]=" << q << " ";
  }
  std::cout << std::endl;
}

int main ()
{
  test_scxx();
  test_PythonAPI();
}
