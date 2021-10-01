/// @file timeDiscr.cpp
///
/// Initialization, allocation  and cleanup of data related
/// to the temporal discretization.
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

#include <cstdlib>
#include "timeDiscr.h"

/// Initializes data of temporal discretization.
///
TimeDiscr::TimeDiscr()
{
  timeStepping = TimeStepping::Local;

  nIterIrs = 2;
  nrk      = 5;
  cfl      = 1.0;
  epsIrs   = 0.3;

  for (int i=0; i<5; i++)
  {
    ark[i]      = 0.0;
    betrk[i]    = 1.0;
    dissipOn[i] = true;
  }

  // private variables

  cvOld   = nullptr;
  tstep   = nullptr;
  rhsOld  = nullptr;
  rhsIter = nullptr;
  nContr  = nullptr;
}

//*****************************************************************************

/// Finishes and cleans up the memory.
///
TimeDiscr::~TimeDiscr()
{
  delete[] cvOld;
  delete[] tstep;
  delete[] rhsOld;
  delete[] rhsIter;
  delete[] nContr;
}

//*****************************************************************************

/// Allocates all necessary memory.
///
/// @param nNodes  total number of grid nodes
/// @exception std::bad_alloc  failed memory allocation
///
void TimeDiscr::AllocateMemory( int nNodes )
{
  cvOld = new CONSVARS[nNodes];
  tstep = new REAL[nNodes];

  if (epsIrs > 0.0)
  {
    rhsOld  = new CONSVARS[nNodes];
    rhsIter = new CONSVARS[nNodes];
    nContr  = new int[nNodes];
  }
}
