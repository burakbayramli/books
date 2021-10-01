/// @file generateEdgelist.cpp
///
/// Generation of temporary and of final edge list.
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
#include <stdexcept>
#include "geometry.h"

using namespace std;

/// Generates temporary and final edge lists. Computes the total number of edges
/// (interior + dummy). The temporary edge list is used in function ComputeMetrics
/// and then deleted. The final edge list (Geometry::edge) is utilized for the
/// computation of fluxes.
///
/// @exception std::runtime_error  failed logic check
/// @exception std::bad_alloc      failed memory allocation
///
void Geometry::GenerateEdgelist()
{
  bool  quit;
  int   d, i, j, ibn, ie, it, n;
  EDGEI *point, *prev;

  // generate temporary edge list *********************************************
  // allocate memory for temporary edge list and reset all pointers

  tmpElist = new EDGELIST[nndInt];

  for (i=0; i<nndInt; i++) {
    tmpElist[i].list = nullptr;
  }

  // loop over nodes of all triangles

  nedInt = 0;

  for (n=0; n<3; n++)
  {
    // loop over triangles
    for (it=0; it<nTria; it++)
    {
      i = tria[it].node[n];
      if (n < 2)
        j = tria[it].node[n+1];
      else
        j = tria[it].node[0];
      if (i > j)  // lower index first
      {
        d = i; i = j; j = d;
      }
      if (tmpElist[i].list == nullptr)     // define new edge (first to node i)
      {
        nedInt++;
        tmpElist[i].list       = new EDGEI;
        tmpElist[i].list->j    = j;
        tmpElist[i].list->edge = -777;
        tmpElist[i].list->next = nullptr;
      }
      else if (tmpElist[i].list->j != j)   // insert "j" into the list
      {
        point = tmpElist[i].list->next;
        prev  = tmpElist[i].list;
        quit  = false;
        do {
          if (point == nullptr)
          {
            nedInt++;
            point       = new EDGEI;
            point->j    = j;
            point->edge = -777;
            point->next = nullptr;
            prev->next  = point;
          }
          if (point->j == j) quit = true;  // edge already stored
          prev  = point;
          point = point->next;
        } while (!quit);
      }
    } // it
  } // n

  // set total number of edges (add edges to dummy nodes)

  nEdges = nedInt + (nNodes-nndInt);

  // generate final edge list *************************************************
  // allocate memory

  edge = new EDGE[nEdges];

  ie = 0;  // edge counter

  // copy edges into final edge list

  for (i=0; i<nndInt; i++)
  {
    point = tmpElist[i].list;
    while (point != nullptr)
    {
      edge[ie].i  = i;
      edge[ie].j  = point->j;
      point->edge = ie;     // we need it in FaceVectorsVolumes
      point       = point->next;
      ie++;
    }
  }

  if (ie != nedInt)
    throw runtime_error( "did not get the correct number of interior edges." );

  // add edges to dummy nodes; store 'dummy' edges in bnode[].edge

  for (ibn=0; ibn<nBnodes; ibn++)
  {
    if (bnode[ibn].edge == -1)            // dummy node here (see DummyNodes)
    {
      edge[ie].i      = bnode[ibn].node;  // boundary node first
      edge[ie].j      = bnode[ibn].dummy; // dummy node second
      bnode[ibn].edge = ie;
      ie++;
    }
  }

  if (ie != nEdges)
    throw runtime_error( "did not get the correct number of dummy edges." );
}