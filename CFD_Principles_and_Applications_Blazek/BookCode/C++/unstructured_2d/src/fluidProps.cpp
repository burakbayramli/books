/// @file fluidProps.cpp
///
/// Initialization, allocation and cleanup of fluid properties.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 1, 2014
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

#include <cstdlib>
#include <cstdio>
#include "fluidProps.h"

/// Initializes variables related to fluid properties.
///
FluidProps::FluidProps()
{
  equsType = Equations::Euler;
  gamma    = 1.4;
  cpgas    = 1004.5;
  prlam    = 0.72;
  renum    = 5000.;
  refVel   = 50.0;
  refRho   = 1.2098;
  refVisc  = 1.78e-5;

  cv    = nullptr;
  dv    = nullptr;
  dvLam = nullptr;
}

//*****************************************************************************

/// Finishes and cleans up the memory.
///
FluidProps::~FluidProps()
{
  delete[] cv;
  delete[] dv;
  delete[] dvLam;
}

//*****************************************************************************

/// Allocates all necessary memory.
///
/// @param nNodes  total number of grid nodes
/// @exception std::bad_alloc  failed memory allocation
///
void FluidProps::AllocateMemory( int nNodes )
{
  cv = new CONSVARS[nNodes];
  dv = new DEPVARS[nNodes];

  if (equsType==Equations::NavierStokes)
    dvLam = new VISCDEPVARS[nNodes];
}