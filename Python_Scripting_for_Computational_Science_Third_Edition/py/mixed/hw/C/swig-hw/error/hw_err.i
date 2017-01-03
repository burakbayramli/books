/* file: hw_err.i */
/* this module has a wrong hw3 interface, see hw.i for a correct one */
%module hw
%{
/* include C header files necessary to compile the interface */
/* not required here, but typically
#include "hw.h"
*/
%}

%include "hw.h"
