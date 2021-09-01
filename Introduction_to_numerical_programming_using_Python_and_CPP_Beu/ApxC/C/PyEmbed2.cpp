// Calling Python functions with vector arguments from C++ code
#include <iostream>
#include <Python.h>

int main()
{
   PyObject *pModule, *pFunc, *pArgs, *pValue;  // pointers to Python objects
   double x[10];
   int i, n;
   PyObject *px;                                    // pointer to Python list

   Py_Initialize();                                      // initialize Python
                                             // import module "operations.py"
   pModule = PyImport_Import(PyUnicode_FromString("operations"));
   if (!pModule) std::cout << "Failed to load <operations.py>";
                                         // get reference to function Average
   pFunc = PyObject_GetAttrString(pModule, "Average");

   n = 5;                                           // values to be averaged:
   for (i=1; i<=n; i++) { x[i] = 0.1 * i; }                   // x[1] to x[n]
                                                       // prepare Python list
   px = PyList_New(n+1);      // pointer to list (offset 0 => n+1 components)
   for (i=1; i<=n; i++) {
      PyList_SetItem(px, i, PyFloat_FromDouble(x[i]));       // populate list
   }

   pArgs = PyTuple_New(2);                     // define tuple of 2 arguments
   PyTuple_SetItem(pArgs, 0, px);                         // set 1st argument
   PyTuple_SetItem(pArgs, 1, PyLong_FromLong(n));         // set 2nd argument

   pValue = PyObject_CallObject(pFunc, pArgs);        // call Python function

   std::cout << " Average = " << PyFloat_AsDouble(pValue);

   Py_DECREF(pArgs);                                              // clean up
   Py_DECREF(pFunc);
   Py_DECREF(pModule);

   Py_Finalize();                                            // finish Python
}
