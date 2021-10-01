/// @file spaceDiscr.cpp
///
/// Initialization, allocation and cleanup of data related
/// to the spatial discretization.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 11, 2014
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
#include "spaceDiscr.h"

/// Initializes data of spatial discretization.
///
SpaceDiscr::SpaceDiscr()
{
  order   = 1;
  limfac  = 1.0;
  epsEntr = 0.05;

  // private variables

  volRef       = 1.0;
  limRef.dens  = 1.0;
  limRef.uvel  = 1.0;
  limRef.vvel  = 1.0;
  limRef.press = 1.0;

  diss   = nullptr;
  rhs    = nullptr;
  lim    = nullptr;
  gradx  = nullptr;
  grady  = nullptr;
  gradTx = nullptr;
  gradTy = nullptr;
  umin   = nullptr;
  umax   = nullptr;
}

//*****************************************************************************

/// Finishes and cleans up the memory.
///
SpaceDiscr::~SpaceDiscr()
{
  delete[] diss;
  delete[] rhs;
  delete[] lim;
  delete[] gradx;
  delete[] grady;
  delete[] gradTx;
  delete[] gradTy;
  delete[] umin;
  delete[] umax;
}

//*****************************************************************************

/// Allocates all necessary memory.
///
/// @param equsType  equations solved (Euler or Navier-Stokes)
/// @param nNodes    total number of grid nodes
/// @exception std::bad_alloc  failed memory allocation
///
void SpaceDiscr::AllocateMemory( Equations equsType, int nNodes )
{
  diss = new CONSVARS[nNodes];
  rhs  = new CONSVARS[nNodes];

  if (order > 1)  // higher-order scheme
  {
    lim   = new PRIMVARS[nNodes];
    gradx = new PRIMVARS[nNodes];
    grady = new PRIMVARS[nNodes];
    umin  = new PRIMVARS[nNodes];
    umax  = new PRIMVARS[nNodes];
    if (equsType==Equations::NavierStokes)
    {
      gradTx = new REAL[nNodes];
      gradTy = new REAL[nNodes];
    }
  }
  else  // first-order scheme
  {
    if (equsType==Equations::NavierStokes)
    {
      gradTx = new REAL[nNodes];
      gradTy = new REAL[nNodes];
    }
  }
}