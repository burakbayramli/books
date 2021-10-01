/// @file solver.cpp
///
/// Initialization of data related to the flow solver.
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

#include "solver.h"

/// Initializes variables related to flow region and to convergence history.
///
Solver::Solver()
{
  restUse = false;
  maxIter = 0;
  outStep = 7777;
  iter    = 0;
  convTol = 1.e-5;
 
  fnameRsti.clear();
  fnameRsto.clear();

  // private variables

  drho    = 0.;
  drho1   = 0.;
  cl      = 0.;
  cd      = 0.;
  cm      = 0.;
  mflow   = 0.;
  mfratio = 1.;
}