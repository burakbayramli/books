/* file: hw.i */
%module hw
%{
/* include C++ header files necessary to compile the interface */
#include "HelloWorld.h"
#include "HelloWorld2.h"
%}

%include "HelloWorld.h"

%include "typemaps.i"
%apply double* OUTPUT { double& s_ }
%include "HelloWorld2.h"

%extend HelloWorld {
    void print_() { self->message(std::cout); }
}
