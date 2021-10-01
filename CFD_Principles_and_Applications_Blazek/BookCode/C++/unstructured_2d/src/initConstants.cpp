/// @file initConstants.cpp
///
/// Initialization of constant values.
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
#include "solver.h"

/// Initializes constants used by the solver.
///
void Solver::InitConstants()
{
  REAL gam1, rgas, temp, mach;

  gam1 = fluidProps.gamma - 1.0;
  rgas = gam1*fluidProps.cpgas/fluidProps.gamma;

  // external flow; it is assumed that "gamma" and "cpgas" specified in
  // the input file are valid for the complete far-field boundary
  if (bndConds.flowType == FlowType::External)
  {
    bndConds.alpha    = bndConds.alpha*RAD;
    bndConds.rhoinf   = bndConds.pinf/(rgas*bndConds.tinf);
    bndConds.qinf     = bndConds.machinf * SQRT(gam1*fluidProps.cpgas*bndConds.tinf);
    bndConds.uinf     = bndConds.qinf*COS(bndConds.alpha);
    bndConds.vinf     = bndConds.qinf*SIN(bndConds.alpha);
    precond.machRef2  = bndConds.machinf*bndConds.machinf;
    fluidProps.refRho = bndConds.rhoinf;
    fluidProps.refVel = bndConds.qinf;
    if (fluidProps.equsType == Equations::NavierStokes)
      fluidProps.refVisc = bndConds.rhoinf*bndConds.qinf*geometry.cref/fluidProps.renum;
    else
      fluidProps.refVisc = 0.0;
  }
  // internal flow
  else
  {
    temp = bndConds.ttinl * POW(bndConds.pout/bndConds.ptinl,gam1/fluidProps.gamma);
    mach = SQRT(2.0*((bndConds.ttinl/temp)-1.0)/gam1);

    bndConds.betainl = bndConds.betainl*RAD;
    bndConds.betaout = bndConds.betaout*RAD;
    precond.machRef2 = mach*mach;
    if (fluidProps.equsType == Equations::NavierStokes)
      fluidProps.refVisc = fluidProps.refRho*fluidProps.refVel*geometry.cref/fluidProps.renum;
    else
      fluidProps.refVisc = 0.0;
  }
}