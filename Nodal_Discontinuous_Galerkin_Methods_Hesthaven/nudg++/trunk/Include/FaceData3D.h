// FaceData3D.h
// 
// 2007/10/04
//---------------------------------------------------------
#ifndef NDG__FaceData3D_H__INCLUDED
#define NDG__FaceData3D_H__INCLUDED

#include "Poly3D.h"
#include "VecObj_Type.h"
#include "MatObj_Type.h"


const int MAXfd = 20;

//---------------------------------------------------------
class FaceData3D
//---------------------------------------------------------
{
public:

  FaceData3D();
  ~FaceData3D();
  void reset();
  FaceData3D& operator=(const FaceData3D& B);

public:

  IV       neighs;          // ids of neighbor faces
  DV      *fx[MAXfd], 
          *fy[MAXfd], 
          *fz[MAXfd];       // {x,y,z} for all points of each face
//DM     fxyz[MAXfd];       // {x,y,z} for all points of each face
  DM      *mmM[MAXfd], 
          *mmP[MAXfd];      // internal,external mass matrices
  Poly3D  *polys[MAXfd];
  DV      *weights[MAXfd];
  int      bcflag;
};


// define FaceMat
typedef MatObj<FaceData3D*> FaceMat;


#endif  // NDG__FaceData3D_H__INCLUDED
