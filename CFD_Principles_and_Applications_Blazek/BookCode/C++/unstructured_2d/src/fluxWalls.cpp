/// @file fluxWalls.cpp
///
/// Computation of fluxes at solid walls.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 7, 2014
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

#include "spaceDiscr.h"

/// Computes convective fluxes in the normal direction at solid walls.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
///
void SpaceDiscr::FluxWalls( const Geometry &geometry, const FluidProps &fluidProps )
{
  int  i, j, ib, ibf, ibegf, iendf;
  REAL sx, sy, pl, pr;

  DEPVARS *dv  = fluidProps.dv;
  NODE    *sbf = geometry.sbf;

  ibegf = 0;
  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendf = geometry.ibound[ib].bfaceIndex;

    if (geometry.btype[ib]>=300 && geometry.btype[ib]<500)
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        i  = geometry.bface[ibf].node1;
        j  = geometry.bface[ibf].node2;
        sx = sbf[ibf].x/12.0;
        sy = sbf[ibf].y/12.0;
        pl = 5.0*dv[i].press +     dv[j].press;
        pr =     dv[i].press + 5.0*dv[j].press;
        rhs[i].xmom += sx*pl;
        rhs[i].ymom += sy*pl;
        rhs[j].xmom += sx*pr;
        rhs[j].ymom += sy*pr;
      }
    }
    ibegf = iendf + 1;
  }
}