// Calling Python functions via C++ wrapper classes
#include <iostream>
#include <Python.h>
//===========================================================================
class Python                          // wrapper class for Python interpreter
{
   public:
   Python() { Py_Initialize(); }            // constructor: initialize Python
   ~Python() { Py_Finalize(); }                  // destructor: finish Python
};
//===========================================================================
class PyOperations                  // wrapper class for module operations.py
{
   Python* contxt;
   PyObject* pModule;

   public:
   PyOperations()                                              // constructor
   {
      contxt = new Python();                 // create new instance of Python
      pModule = PyImport_Import(PyUnicode_FromString("operations"));
      if (!pModule) std::cout << "Failed to load <operations.py>";
   }
   virtual ~PyOperations()                                      // destructor
   {
      if (pModule) Py_DECREF(pModule);
      if (contxt) delete contxt;
   }
//---------------------------------------------------------------------------
   double Sum(double x, double y)          // wrapper for Python function Sum
   {
      PyObject *pFunc, *pArgs, *pValue;
                                                      // get reference to Sum
      pFunc = PyObject_GetAttrString(pModule, "Sum");

      pArgs = PyTuple_New(2);                  // define tuple of 2 arguments
      PyTuple_SetItem(pArgs, 0, PyFloat_FromDouble(x));   // set 1st argument
      PyTuple_SetItem(pArgs, 1, PyFloat_FromDouble(y));   // set 2nd argument

      pValue = PyObject_CallObject(pFunc, pArgs);     // call Python function

      Py_DECREF(pArgs);                                           // clean up
      Py_DECREF(pFunc);  

      return PyFloat_AsDouble(pValue);                       // return result
   }
};

int main()
{
   PyOperations c;                             // object of type PyOperations
   double x, y;

   x = 2.0, y = 3.0;                                    // values to be added
   std::cout << x << " + " << y << " = " << c.Sum(x,y);    // call method Sum
}
