/// @file geometry.cpp
///
/// Initialization and cleanup of the geometrical data.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 28, 2014
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
#include "geometry.h"

/// Initializes variables related to grid geometry.
///
Geometry::Geometry()
{
  nNodes  = 0;
  nndInt  = 0;
  nEdges  = 0;
  nedInt  = 0;
  nTria   = 0;
  nSegs   = 0;
  nBfaces = 0;
  nBnodes = 0;

  xref = 0.0;
  yref = 0.0;
  cref = 1.0;

  bname  = nullptr;
  bface  = nullptr;
  bnode  = nullptr;
  ibound = nullptr;
  tria   = nullptr;
  edge   = nullptr;
  coords = nullptr;
  sij    = nullptr;
  vol    = nullptr;
  sbf    = nullptr;
  sproj  = nullptr;

  fnameGrid.clear();

  // private variables

  tmpElist = nullptr;
}

//*****************************************************************************

/// Finishes and cleans up the memory.
///
Geometry::~Geometry()
{
  delete[] bname;
  delete[] bface;
  delete[] bnode;
  delete[] ibound;
  delete[] tria;
  delete[] edge;
  delete[] coords;
  delete[] sij;
  delete[] vol;
  delete[] sbf;
  delete[] sproj;

  // private variables
  
  DeleteTmpElist();
}
