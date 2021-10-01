/// @file faceVectorsSymm.cpp
///
/// Correction of face vectors at symmetry boundaries.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 27, 2014
// 
//=============================================================================
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//
//*****************************************************************************

#include "defs.h"
#include "geometry.h"

/// Corrects face vectors of edges which represent the symmetry boundary.
/// The reason is that there must be no component in the direction normal
/// to the boundary for the fluxes to be computed correctly. Function also
/// changes the boundary type to reflect the orientation of the symmetry
/// boundary: 501 for symmetry along x-direction, 502 along y-direction.
///
/// @exception std::bad_alloc  failed memory allocation
///
void Geometry::FaceVectorsSymm()
{
  int  i, j, ib, ibf, ibn, ie, ibegf, ibegn, iendf, iendn;
  int  *marker;
  REAL sx, sy;

  //  mark nodes at symmetry boundaries

  marker = new int[nndInt];
  for (i=0; i<nndInt; i++)
  {
    marker[i] = -1;
  }

  ibegf = 0;
  ibegn = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendf = ibound[ib].bfaceIndex;
    iendn = ibound[ib].bnodeIndex;
    if (btype[ib]>=500 && btype[ib]<600)
    {
      sx = 0.0;
      sy = 0.0;
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        sx += sbf[ibf].x;
        sy += sbf[ibf].y;
      }
      if (ABS(sx) > ABS(sy))
        btype[ib] = 501;      // symmetry at x=const. plane
      else
        btype[ib] = 502;      // symmetry at y=const. plane
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        marker[bnode[ibn].node] = btype[ib] - 500;  // store symmetry axis
      }
    }
    ibegf = iendf + 1;
    ibegn = iendn + 1;
  }

  // correct face vectors where necessary

  for (ie=0; ie<nedInt; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;
    if (marker[i]!=-1 && marker[j]!=-1)
    {
      if (marker[i] < 2)     // x=const. plane
        sij[ie].x = 0.0;
      else                   // y=const. plane
        sij[ie].y = 0.0;
    }
  }

  delete[] marker;
}