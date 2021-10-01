/// @file writeSolution.cpp
///
/// Output of flow solution (in binary format).
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

/// Stores the current flow solution together with the initial residual
/// and the actual number of iterations.
///
/// @exception std::runtime_error  file cannot be opened
///
void Solver::WriteSolution()
{
  // open file

  ofstream stream( fnameRsto,ios::binary | ios::trunc,_SH_DENYRW );
  if (stream.fail()) throw runtime_error( "could not open solution file for writing." );

  // dimension (for checking purposes)

  WriteBinary( stream,geometry.nNodes );

  // initial residual, iteration number and solution

  WriteBinary( stream,drho1 );
  WriteBinary( stream,iter  );

  for (int i=0; i<geometry.nNodes; i++)
  {
    WriteBinary( stream,fluidProps.cv[i] );
  }

  stream.close();
}