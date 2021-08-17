// FaceData3D.cpp
// 
// 2007/10/04
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include "FaceData3D.h"


//---------------------------------------------------------
FaceData3D::FaceData3D() 
//---------------------------------------------------------
{
  bcflag = 0;

  // testing pointer version: init to NULL
  for (int i=0; i<MAXfd; ++i) 
  {
     fx[i]=NULL; 
     fy[i]=NULL; 
     fz[i]=NULL;
    mmM[i]=NULL;
    mmP[i]=NULL; 

    polys  [i]=NULL;
    weights[i]=NULL;
  }
}


//---------------------------------------------------------
FaceData3D::~FaceData3D() 
//---------------------------------------------------------
{ 
  reset(); 
}


//---------------------------------------------------------
void FaceData3D::reset() 
//---------------------------------------------------------
{
  neighs.resize(0);   // clear neighbor list
  bcflag = 0;         // clear boundary flag

  // testing pointer version: delete arrays of pointers
  for (int i=0; i<MAXfd; ++i) {
    if ( fx[i]) { delete  fx[i];      fx[i]=NULL; }
    if ( fy[i]) { delete  fy[i];      fy[i]=NULL; }
    if ( fz[i]) { delete  fz[i];      fz[i]=NULL; }
    if (mmM[i]) { delete mmM[i];     mmM[i]=NULL; }
    if (mmP[i]) { delete mmP[i];     mmP[i]=NULL; }

    if (polys  [i]) { delete polys  [i]; polys  [i]=NULL; }
    if (weights[i]) { delete weights[i]; weights[i]=NULL; }
  }
}


//---------------------------------------------------------
FaceData3D& FaceData3D::operator=(const FaceData3D& B) 
//---------------------------------------------------------
{ 
  return (*this); 
}
