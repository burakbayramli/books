/// @file limiterRefVals.cpp
///
/// Initialization of limiter reference values.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 3, 2014
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

/// Computes reference values of limited variables (density, u, v, pressure)
/// and of the control volumes. The reference values are used to normalize
/// variables within the limiter functions (Roe's upwind scheme).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param bndConds    boundary conditions
///
void SpaceDiscr::LimiterRefvals( const Geometry &geometry, const FluidProps &fluidProps,
                                 const BndConds &bndConds )
{
  REAL gam1, rgas, temp, rho, cs, mach;

  // reference volume (= largest control volume)

  volRef = -1.0e+32;
  for (int i=0; i<geometry.nndInt; i++)
  {
    volRef = MAX(volRef,geometry.vol[i]);
  }

  // reference density, velocity and pressure

  gam1 = fluidProps.gamma - 1.0;
  rgas = gam1*fluidProps.cpgas/fluidProps.gamma;

  // external flow
  if (bndConds.flowType == FlowType::External)
  {
    limRef.dens  = bndConds.rhoinf;
    limRef.uvel  = SQRT(bndConds.uinf*bndConds.uinf+bndConds.vinf*bndConds.vinf);
    limRef.vvel  = limRef.uvel;
    limRef.press = bndConds.pinf;
  }
  // internal flow
  else
  {
    temp         = bndConds.ttinl * POW(bndConds.pout/bndConds.ptinl,gam1/fluidProps.gamma);
    rho          = bndConds.pout/(rgas*temp);
    cs           = SQRT(fluidProps.gamma*bndConds.pout/rho);
    mach         = SQRT(2.0*((bndConds.ttinl/temp)-1.0)/gam1);
    limRef.dens  = rho;
    limRef.uvel  = mach*cs;
    limRef.vvel  = limRef.uvel;
    limRef.press = bndConds.pout;
  }
}