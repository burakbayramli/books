/// @file boundaryConditions.cpp
///
/// Application of boundary conditions.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 2, 2014
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

/// Sets boundary conditions at dummy points. In a first loop, b.c.'s at
/// inlet, outlet and far-field are specified. In a second loop, b.c.'s are
/// set for all solid walls.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param precond     low Mach-number preconditioning
///
void BndConds::BoundaryConditions( const Geometry &geometry, FluidProps &fluidProps,
                                   const Precond &precond )
{
  int ib, ibegn, iendn, itype;

  // loop over all boundaries with the exception of walls

  ibegn = 0;
  for (ib=0; ib<geometry.nSegs; ib++)
  {
    itype = geometry.btype[ib];
    iendn = geometry.ibound[ib].bnodeIndex;

    // inflow
    if (itype>=100 && itype<200)
    {
      BcondInflow( geometry,fluidProps,ibegn,iendn );
    }
    // outflow
    else if (itype>=200 && itype<300)
    {
      BcondOutflow( geometry,fluidProps,ibegn,iendn );
    }
    // far-field
    else if (itype>=600 && itype<700)
    {
      BcondFarfield( geometry,fluidProps,precond,ibegn,iendn );
    }
    ibegn = iendn + 1;
  }

  // solid walls (treated last because they should dominate)

  ibegn = 0;
  for (ib=0; ib<geometry.nSegs; ib++)
  {
    itype = geometry.btype[ib];
    iendn = geometry.ibound[ib].bnodeIndex;

    // viscous (no-slip) wall - if Navier-Stokes equations solved
    if (fluidProps.equsType==Equations::NavierStokes && (itype>=300 && itype<400))
    {
      BcondWallVisc( geometry,fluidProps,ibegn,iendn );
    }
    ibegn = iendn + 1;
  }
}