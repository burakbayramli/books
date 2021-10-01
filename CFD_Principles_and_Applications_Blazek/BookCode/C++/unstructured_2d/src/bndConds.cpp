/// @file bndConds.cpp
///
/// Initialization, allocation and cleanup of data related
/// to the boundary conditions.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 30, 2014
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
#include "bndConds.h"

/// Initializes variables related to boundary conditions.
///
BndConds::BndConds()
{
  flowType = FlowType::External;

  // external flow

  vortCorr = false;
  machinf  = 0.147;
  alpha    = 0.0;
  pinf     = 1.0e+5;
  tinf     = 288.0;
  rhoinf   = 1.2098;
  uinf     = 50.0;
  vinf     = 0.0;
  qinf     = SQRT(uinf*uinf+vinf*vinf);
  cl       = 0.0;
  bndVals  = nullptr;

  // internal flow

  ptinl   = 1.0e+5;
  ttinl   = 288.0;
  betainl = 0.0;
  betaout = 0.0;
  p12rat  = 1.0;
  pout    = 0.9e+5;
}

//*****************************************************************************

/// Finishes and cleans up the memory.
///
BndConds::~BndConds()
{
  delete[] bndVals;
}

//*****************************************************************************

/// Allocates all necessary memory.
///
/// @param geometry  geometrical data (number of boundary nodes at far-field)
/// @exception std::bad_alloc  failed memory allocation
///
void BndConds::AllocateMemory( const Geometry &geometry )
{
  if (flowType == FlowType::Internal) return;  // nothing to allocate

  int nbFarf = geometry.GetNumberBoundNodes( 600,699 );  // no. of nodes at far-field boundary

  bndVals = new PRIMVARS[nbFarf];
}