/* file: ext_gridloop.i */
%module ext_gridloop
%{
#include "gridloop.h"
#include <numpy/arrayobject.h>  // needed for import_array macro
%}

%typemap(in) PyArrayObject* {
   $1 = (PyArrayObject*) $input;
}
%include "gridloop.h"

%init%{
    import_array();
%}

