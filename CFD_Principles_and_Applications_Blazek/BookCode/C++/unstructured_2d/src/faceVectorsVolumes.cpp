/// @file faceVectorsVolumes.cpp
///
/// Computation of face vectors and volumes.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 27, 2014
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

#include <stdexcept>
#include "defs.h"
#include "geometry.h"

using namespace std;

/// Computes face vectors and control volumes. Values at the boundaries
/// are finished / updated later in Geometry::FaceVectorsVolumesBound.
///
/// @exception std::runtime_error  failed logic check
///
void Geometry::FaceVectorsVolumes()
{
  int   d, i, j, ie, it, n;
  REAL  x1, y1, x2, y2, x3, y3, area, pvol, cx, cy, sx, sy, vprod;
  EDGEI *point;

  // zero out face vectors and volumes

  for (ie=0; ie<nEdges; ie++)
  {
    sij[ie].x = 0.0;
    sij[ie].y = 0.0;
  }
  for (i=0; i<nNodes; i++)
  {
    vol[i] = 0.0;
  }

  // loop over triangles, compute volumes and face vectors

  for (it=0; it<nTria; it++)
  {
    // triangle area

    x1 = coords[tria[it].node[0]].x;
    y1 = coords[tria[it].node[0]].y;
    x2 = coords[tria[it].node[1]].x;
    y2 = coords[tria[it].node[1]].y;
    x3 = coords[tria[it].node[2]].x;
    y3 = coords[tria[it].node[2]].y;

    area = 0.5*((x1-x2)*(y1+y2)+(x2-x3)*(y2+y3)+(x3-x1)*(y3+y1));
    pvol = ABS(area)/3.0;

    // distribute area to the corner nodes (1/3 to each)

    vol[tria[it].node[0]] += pvol;
    vol[tria[it].node[1]] += pvol;
    vol[tria[it].node[2]] += pvol;

    // compute center of the triangle

    cx = (x1+x2+x3)/3.0;
    cy = (y1+y2+y3)/3.0;

    // loop over individual nodes

    for (n=0; n<3; n++)
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

      // compute part of face vector associated with edge ij;
      // orient it to point in direction from i to j

      sx =  cy - 0.5*(coords[i].y+coords[j].y);
      sy = -cx + 0.5*(coords[i].x+coords[j].x);

      vprod = sx*(coords[j].x-coords[i].x) + sy*(coords[j].y-coords[i].y);
      if (vprod < 0.0)
      {
        sx = -sx; sy = -sy;
      }

      // search corresponding edge and add (sx,sy) to sij[]

      point = tmpElist[i].list;
      while (point != nullptr)
      {
        if (point->j == j)
        {
          ie = point->edge;
          sij[ie].x += sx;
          sij[ie].y += sy;
          break;
        }
        point = point->next;
      }
      if (point == nullptr) throw runtime_error( "could not find edge to a node." );

    } // loop over nodes of a triangle

  } // loop over triangles
}