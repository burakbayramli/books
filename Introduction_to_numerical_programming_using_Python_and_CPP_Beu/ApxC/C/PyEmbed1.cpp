// Calling Python functions with scalar arguments from C++ code
#include <iostream>
#include <Python.h>

int main()
{
   PyObject *pModule, *pFunc, *pArgs, *pValue;  // pointers to Python objects
   double x, y;

   x = 2.0; y = 3.0;                                    // values to be added
   
   Py_Initialize();                                      // initialize Python
                                             // import module "operations.py"
   pModule = PyImport_Import(PyUnicode_FromString("operations"));
   if (!pModule) std::cout << "Failed to load <operations.py>";
                                             // get reference to function Sum
   pFunc = PyObject_GetAttrString(pModule, "Sum");

   pArgs = PyTuple_New(2);                     // define tuple of 2 arguments
   PyTuple_SetItem(pArgs, 0, PyFloat_FromDouble(x));      // set 1st argument
   PyTuple_SetItem(pArgs, 1, PyFloat_FromDouble(y));      // set 2nd argument

   pValue = PyObject_CallObject(pFunc, pArgs);        // call Python function

   std::cout << x << " + " << y << " = " << PyFloat_AsDouble(pValue);

   Py_DECREF(pArgs);                                              // clean up
   Py_DECREF(pFunc);
   Py_DECREF(pModule);

   Py_Finalize();                                            // finish Python
}
