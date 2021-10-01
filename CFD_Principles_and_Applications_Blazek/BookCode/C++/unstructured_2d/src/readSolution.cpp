/// @file readSolution.cpp
///
/// Input of flow solution (in binary format).
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
#include <fstream>
#include <iostream>
#include <stdexcept>
#include "solver.h"
#include "streamIO.h"

using namespace std;

/// Reads in previous solution in order to restart the simulation. It also
/// reads the initial residual and the number of previous iterations.
///
/// @exception std::runtime_error  failed logic check or read problem
///
void Solver::ReadSolution()
{
  int i, nNodesDum;

  // open file

  ifstream stream( fnameRsti,ios::binary );
  if (stream.fail()) throw runtime_error( "could not open solution file for reading." );

  // dimension (for checking purposes)

  ReadBinary( stream,nNodesDum );
  if (nNodesDum != geometry.nNodes)
    throw runtime_error( "number of nodes differs from the grid file." );

  // initial residual, iteration number and solution

  ReadBinary( stream,drho1 );
  ReadBinary( stream,iter  );

  for (i=0; i<geometry.nNodes; i++)
  {
    ReadBinary( stream,fluidProps.cv[i] );
  }

  stream.close();
}