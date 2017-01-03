/* file: ext_gridloop.i */
%module ext_gridloop
%{
/* include C++ header files necessary to compile the interface */
#include "convert.h"
#include "gridloop.h"
//#include <numpy/arrayobject.h>  // needed for import_array macro
%}

%include "convert.h"
%include "gridloop.h"

/* put in class constructor instead:
%init%{
    import_array();
%}
*/
