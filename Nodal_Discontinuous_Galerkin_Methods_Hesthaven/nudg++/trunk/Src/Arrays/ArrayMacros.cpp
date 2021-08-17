// ArrayMacros.cpp
// 
// 2006/12/12
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "ArrayMacros.h"

// global objects passed to array constructors to help 
// distinguish orientation when loading ASCII data.
ArrayData    gVecData;
MatRowData   gRowData;
MatColData   gColData;

// Object to help Region2D return a matrix column/row
MatDimension   All;

