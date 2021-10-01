/// @file dummyNodes.cpp
///
/// Generation of dummy nodes at inlet, outlet and far-field boundaries.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 20, 2014
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
#include <iostream>
#include <stdexcept>
#include <sstream>
#include "geometry.h"

using namespace std;

/// Stores indexes of dummy nodes in "bnode" -> index of boundary node,
/// index of dummy node. Adds number of dummy nodes to number of physical
/// (interior) nodes.
///
/// @exception std::runtime_error  failed logic check
/// @exception std::bad_alloc      failed memory allocation
///
void Geometry::DummyNodes()
{
  bool flag, *marker;
  int  i, ib, ibf, idn, ibegf, iendf, ibegn, iendn, itype;

  marker = new bool[nndInt];  // marker of boundary nodes (inlet/outlet/far-field)

  // loop over boundary segments

  ibegf = 0;
  ibegn = 0;
  idn   = 0;  // counter of dummy nodes

  for (ib=0; ib<nSegs; ib++)
  {
    iendf = ibound[ib].bfaceIndex;
    iendn = ibound[ib].bnodeIndex;
    itype = btype[ib];
    flag  = false;   // true for inlet/outlet/far-field
    if (itype>=100 && itype<200) flag = true;
    if (itype>=200 && itype<300) flag = true;
    if (itype>=600 && itype<700) flag = true;

    if (itype<700 || itype>=800)   // NOT a periodic boundary
    {
      // reset node marker
      for (i=0; i<nndInt; i++)
      {
        marker[i] = false;
      }

      // loop over faces of boundary "ib" and mark nodes
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        marker[bface[ibf].node1] = true;
        marker[bface[ibf].node2] = true;
      }

      // store node indexes in "bnode";
      // count dummy nodes (idn) for inlet/outlet/far-field boundary
      for (i=0; i<nndInt; i++)
      {
        if (marker[i])                         // must be on boundary
        {
          if (ibegn >= nBnodes)                // check dimension
            throw runtime_error( "max. number of boundary nodes exceeded." );
          if (flag)                            // *** inlet/outlet/far-field
          {
            bnode[ibegn].node  = i;            // index of boundary node
            bnode[ibegn].dummy = nndInt + idn; // index of dummy node
            bnode[ibegn].edge  = -1;           // set in "GenerateEdgelist"
            idn++;
          }
          else                                 // *** other boundary type
          {
            bnode[ibegn].node = i;             // index of boundary node
          }
          ibegn++;                             // count boundary nodes
        }
      }

      // check number of boundary nodes
      if ((ibegn-1) != iendn)
      {
        string str = "no. of nodes for boundary " + to_string(itype) +
                     " is wrong. It should be " + to_string(iendn) +
                     " but it is " + to_string(ibegn-1) + ".";
        throw runtime_error( str );
      }
    } // periodic?

    // update pointers to faces and to nodes
    ibegf = iendf + 1;
    ibegn = iendn + 1;
  } // ib

  delete[] marker;

  // set total number of nodes (add dummy nodes)

  nNodes = nndInt + idn;
}