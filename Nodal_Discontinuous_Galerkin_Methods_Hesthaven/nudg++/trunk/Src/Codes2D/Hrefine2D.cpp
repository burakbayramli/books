// Hrefine2D.cpp
// function Hrefine2D(refineflag)
// 2007/07/31
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG2D.h"


//---------------------------------------------------------
void NDG2D::Hrefine2D(IVec& refineflag)
//---------------------------------------------------------
{
  // function Hrefine2D(refineflag)
  // purpose:  apply non-conforming refinement to the set 
  //           of elements labelled in refineflag

  IVec v1,v2,v3, v4,v5,v6, tv4,tv5,tv6, ids,newids; 
  IMat mv1,mv2,mv3; DVec x1,x2,x3, y1,y2,y3;

  // 1.1 Count vertices
  int Nv = VX.length();

  // 1.2 Find and count elements to be refined
  IVec ref = sort(find(refineflag, '!', 0), true);
  int Nrefine = ref.length();

  // 1.3 Extract vertex numbers of elements to refine
  // NBN: extract "Region2D" as IMat, then convert to IVec
  mv1 = EToV(ref,1); mv2 = EToV(ref,2); mv3 = EToV(ref,3); 
  v1 = mv1; v2 = mv2; v3 = mv3;

  IVec range0 = Nfaces*Range(0, K-1);

  // 1.4 Uniquely number all face centers
  v4 = max( 1+range0, EToF.get_col(1)+Nfaces*(EToE.get_col(1)-1) );
  v5 = max( 2+range0, EToF.get_col(2)+Nfaces*(EToE.get_col(2)-1) );
  v6 = max( 3+range0, EToF.get_col(3)+Nfaces*(EToE.get_col(3)-1) );

  // 2.0 Extract face center vertices for elements to refine 
  tv4 = v4(ref); tv5 = v5(ref); tv6 = v6(ref);

  // 2.1 Renumber face centers contiguously from Nv+1
  ids = unique(concat(tv4,tv5,tv6));
  newids.resize(ids.max_val());
  newids(ids) = Range(1,ids.length());
  v4=Nv+newids(tv4); v5=Nv+newids(tv5); v6=Nv+newids(tv6);

  // 2.2 Replace original triangle with triangle connecting edge centers
//EToV(ref,All) = [v4,v5,v6]; 
  EToV(ref,1)=v4.data();
  EToV(ref,2)=v5.data();
  EToV(ref,3)=v6.data();

  IVec v1v2v3 = concat(v1,v2,v3), 
       v4v5v6 = concat(v4,v5,v6),
       v6v4v5 = concat(v6,v4,v5);

  // 3.0 Add extra triangles to EToV
  IVec newR = Range(K+1,K+3*Nrefine);
  EToV.realloc(K+3*Nrefine, 3);

  EToV(newR,1) = v1v2v3.data(); // 1st vertices of new elements
  EToV(newR,2) = v4v5v6.data(); // 2nd vertices of new elements
  EToV(newR,3) = v6v4v5.data(); // 3rd vertices of new elements

  // 3.1 Create boundary condition type for refined elements
  IMat bcsave = BCType(ref,All);
  BCType(ref, All) = 0; // now internal faces

  // set index ranges
  Index1D I1(K+1,K+Nrefine), I2(K+Nrefine+1,K+2*Nrefine), I3(K+2*Nrefine+1,K+3*Nrefine);
  
  // extend BCType to store faces for new elements
  BCType.realloc(K+3*Nrefine, 3);

  BCType(I1, 1) = bcsave(All, 1);
  BCType(I1, 3) = bcsave(All, 3);

  BCType(I2, 1) = bcsave(All, 2);
  BCType(I2, 3) = bcsave(All, 1);

  BCType(I3, 1) = bcsave(All, 3);
  BCType(I3, 3) = bcsave(All, 2);

  // 3.2 Find vertex locations of elements to be refined
  x1 = VX(v1);  x2 = VX(v2);  x3 = VX(v3);    
  y1 = VY(v1);  y2 = VY(v2);  y3 = VY(v3);    

  // extend {VX,VY} to store new vertices
  int max_vid=EToV.max_val(); VX.realloc(max_vid); VY.realloc(max_vid);

  // 3.3 Add coordinates for refined edge centers
  VX(v4) = 0.5*(x1+x2); VX(v5) = 0.5*(x2+x3); VX(v6) = 0.5*(x3+x1); 
  VY(v4) = 0.5*(y1+y2); VY(v5) = 0.5*(y2+y3); VY(v6) = 0.5*(y3+y1); 

  // 3.4 Increase element count
  K = K+3*Nrefine;
}
