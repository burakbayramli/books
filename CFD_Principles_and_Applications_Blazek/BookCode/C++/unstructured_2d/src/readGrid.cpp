/// @file readGrid.cpp
///
/// Input of grid coordinates, elements and boundaries.
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
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <sstream>
#include "geometry.h"
#include "streamIO.h"

using namespace std;

/// Reads in grid data and boundary segments.
///
/// @exception std::runtime_error  failed logic check or read problem
/// @exception std::bad_alloc      failed memory allocation
///
void Geometry::ReadGrid()
{
  int    i, ib, ibn, ibf, ibegf, iendf, ibegn, iendn;
  string str;

  // open file

  ifstream stream( fnameGrid );
  if (stream.fail()) throw runtime_error( "could not open grid file." );

  str = ReadLine( stream );
  str = ReadLine( stream );
  str = ReadLine( stream );

  // numbers of physical nodes, triangles and boundary segments

  str = ReadLine( stream );
  stringstream(str) >> nndInt >> nTria >> nSegs;

  // boundary type, no. of boundary faces & nodes, boundary name

  btype  = new int[nSegs];
  bname  = new string[nSegs];
  ibound = new IBOUND[nSegs];

  str = ReadLine( stream );
  for (ib=0; ib<nSegs; ib++)
  {
    str = ReadLine( stream );
    stringstream(str) >> btype[ib] >> ibound[ib].bfaceIndex >> ibound[ib].bnodeIndex;
    ibound[ib].bfaceIndex--;  // correcting Fortran indexing
    ibound[ib].bnodeIndex--;
    bname[ib] = ReadLine( stream );
  }
  nBfaces = ibound[nSegs-1].bfaceIndex + 1;
  nBnodes = ibound[nSegs-1].bnodeIndex + 1;

  // definition of boundary faces / periodic nodes

  bnode = new BNODE[nBnodes];
  bface = new BFACE[nBfaces];

  for (ibn=0; ibn<nBnodes; ibn++)
  {
    bnode[ibn].node  = -777;
    bnode[ibn].dummy = -777;  // set in DummyNodes
    bnode[ibn].edge  = -777;  // set in EdgesFinalize
  }
  for (ibf=0; ibf<nBfaces; ibf++)
  {
    bface[ibf].node1 = -777;
    bface[ibf].node2 = -777;
  }

  str   = ReadLine( stream );
  ibegf = 0;
  ibegn = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendf = ibound[ib].bfaceIndex;
    iendn = ibound[ib].bnodeIndex;
    if (btype[ib]>=700 && btype[ib]<800)   // periodic nodes
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        str = ReadLine( stream );
        stringstream(str) >> bnode[ibn].node >> bnode[ibn].dummy;
        bnode[ibn].node--;  // correcting Fortran indexing
        bnode[ibn].dummy--;
      }
    }
    else  // boundary faces
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        str = ReadLine( stream );
        stringstream(str) >> bface[ibf].node1 >> bface[ibf].node2;
        bface[ibf].node1--;  // correcting Fortran indexing
        bface[ibf].node2--;
      }
    }
    ibegf = iendf + 1;
    ibegn = iendn + 1;
  }

  // check boundary faces pointer

  for (ibf=0; ibf<nBfaces; ibf++)
  {
    if (bface[ibf].node1<0 || bface[ibf].node2<0)
      throw runtime_error( "incompletely defined array bface[]." );
  }

  // generate dummy nodes

  DummyNodes();

  // read grid nodes

  coords = new NODE[nNodes];

  str = ReadLine( stream );
  for (i=0; i<nndInt; i++)
  {
    str = ReadLine( stream );
    stringstream(str) >> coords[i].x >> coords[i].y;
  }

  // read triangles

  tria = new TRIA[nTria];

  str = ReadLine( stream );
  for (i=0; i<nTria; i++)
  {
    str = ReadLine( stream );
    stringstream(str) >> tria[i].node[0] >> tria[i].node[1] >> tria[i].node[2];
    tria[i].node[0]--;  // correcting Fortran indexing
    tria[i].node[1]--;
    tria[i].node[2]--;
  }

  // close file

  stream.close();
}