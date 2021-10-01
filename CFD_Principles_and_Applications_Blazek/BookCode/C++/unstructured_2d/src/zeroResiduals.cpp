/// @file zeroResiduals.cpp
///
/// Correction of residuals at symmetry and no-slip boundaries.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 8, 2014
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

#include "bndConds.h"

/// Zeros out normal component of the residual at symmetry and
/// at no-slip boundaries.
///
/// @param geometry  geometrical data
/// @param equsType  equations solved (Euler or Navier-Stokes)
/// @param rhs       vector of residuals
///
void BndConds::ZeroResiduals( const Geometry &geometry, Equations equsType, CONSVARS rhs[] )
{
  int i, ib, ibn, ibegn, iendn;

  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendn = geometry.ibound[ib].bnodeIndex;

    // symmetry boundary

    if (geometry.btype[ib]>=500 && geometry.btype[ib]<600)
    {
      if (geometry.btype[ib]-500 < 2) // x=const. line -> x-component
      {
        for (ibn=ibegn; ibn<=iendn; ibn++)
        {
          i = geometry.bnode[ibn].node;
          rhs[i].xmom = 0.0;
        }
      }
      else                            // y=const. line -> y-component
      {
        for (ibn=ibegn; ibn<=iendn; ibn++)
        {
          i = geometry.bnode[ibn].node;
          rhs[i].ymom = 0.0;
        }
      }

    // viscous (no-slip) wall
    }
    else if ((geometry.btype[ib]>=300 && geometry.btype[ib]<400) &&
             equsType==Equations::NavierStokes)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = geometry.bnode[ibn].node;
        rhs[i].xmom = 0.0;       // velocity components = 0
        rhs[i].ymom = 0.0;
      }
    }
    ibegn = iendn + 1;
  } // boundary
}