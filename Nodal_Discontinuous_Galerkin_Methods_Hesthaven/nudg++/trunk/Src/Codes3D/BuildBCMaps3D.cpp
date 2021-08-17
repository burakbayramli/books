// BuildBCMaps3D.m
// function BuildBCMaps3D()
// 2007/07/07
//---------------------------------------------------------
#include "NDGLib_headers.h"
#include "NDG3D.h"


//---------------------------------------------------------
void NDG3D::BuildBCMaps3D()
//---------------------------------------------------------
{
  // function BuildBCMaps3D
  // Purpose: Build specialized nodal maps for various types of
  //          boundary conditions, specified in BCType. 

  // Note: boundary codes are defined in Global_funcs.h

  // create label of face nodes with boundary types from BCType
  IVec bct = trans(BCType), onesNfp(Nfp, 1);
  IMat bnodeM = outer(onesNfp, bct);
  IVec bnodes = bnodeM;

  // find location of boundary nodes in face and volume node lists
  mapI = find(bnodes, '=', (int)BC_In);        vmapI = vmapM(mapI);
  mapO = find(bnodes, '=', (int)BC_Out);       vmapO = vmapM(mapO);
  mapW = find(bnodes, '=', (int)BC_Wall);      vmapW = vmapM(mapW);
  mapF = find(bnodes, '=', (int)BC_Far);       vmapF = vmapM(mapF);
  mapC = find(bnodes, '=', (int)BC_Cyl);       vmapC = vmapM(mapC);
  mapD = find(bnodes, '=', (int)BC_Dirichlet); vmapD = vmapM(mapD);
  mapN = find(bnodes, '=', (int)BC_Neuman);    vmapN = vmapM(mapN);
  mapS = find(bnodes, '=', (int)BC_Slip);      vmapS = vmapM(mapS);
}
