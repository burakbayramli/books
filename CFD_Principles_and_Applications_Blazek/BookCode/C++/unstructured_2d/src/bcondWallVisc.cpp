/// @file bcondWallVisc.cpp
///
/// Treatment of no-slip (viscous) wall boundaries.
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

#include "defs.h"
#include "bndConds.h"

/// Applies no-slip wall boundary condition. Adiabatic walls only
/// are assumed (velocity components are zeroed out).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param ibegn       indirect pointer to first node of the boundary
/// @param iendn       indirect pointer to last node of the boundary
///
void BndConds::BcondWallVisc( const Geometry &geometry, FluidProps &fluidProps,
                              int ibegn, int iendn ) const
{
  int ib, ibn;

  CONSVARS *cv = fluidProps.cv;

  for (ib=ibegn; ib<=iendn; ib++)
  {
    ibn = geometry.bnode[ib].node;   // boundary node

    cv[ibn].xmom = 0.0;
    cv[ibn].ymom = 0.0;

    fluidProps.DependentVarsOne( ibn );
  } // ib
}