/// @file faceVectorsVolumesBound.cpp
///
/// Finalization of face vectors and volumes at the boundaries.
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

#include <stdexcept>
#include "defs.h"
#include "geometry.h"

using namespace std;

/// Duplicates control volumes at periodic boundaries. Computes face vectors
/// normal to boundaries (except periodic ones). Generates face vectors for 
/// dummy nodes (inlet/outlet/far-field). Sets coordinates of dummy nodes
/// equal to those of boundary nodes.
///
/// @exception std::runtime_error  failed logic check
/// @exception std::bad_alloc      failed memory allocation
///
void Geometry::FaceVectorsVolumesBound()
{
  bool flag, *marker;
  int  d, i, j, ibegf, ibegn, iendf, iendn, itype, n1, n2, nt1, nt2, nt3;
  int  ib, ibf, ibn, ie, it, n;
  int  *btria;
  REAL cx, cy, xm, ym, vprod;

  // combine control volumes at periodic nodes ********************************

  ibegn = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendn = ibound[ib].bnodeIndex;
    if (btype[ib]>=700 && btype[ib]<800)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i       = bnode[ibn].node;
        j       = bnode[ibn].dummy;
        vol[i] += vol[j];
        vol[j]  = vol[i];
      }
    }
    ibegn = iendn + 1;
  }

  // compute face vectors at the boundaries ***********************************
  // (find for each boundary face the corresp. triangle - required in order
  // to define the face vectors such as to point OUT of the domain)

  // allocate and clear node marker (nodes touched by bface[]); mark nodes

  marker = new bool[nndInt];

  for (i=0; i<nndInt; i++)
  {
    marker[i] = false;
  }
  for (ibf=0; ibf<nBfaces; ibf++)
  {
    marker[bface[ibf].node1] = true;
    marker[bface[ibf].node2] = true;
  }

  // loop over triangles: if a face = bface[] then store index of the triangle
  // in btria[] => pointer from boundary face to the adjacent element

  btria = new int[nBfaces];
  for (ibf=0; ibf<nBfaces; ibf++)
  {
    btria[ibf] = -1;  // clear
  }

  for (n=0; n<3; n++)                    // loop over nodes of the triangles
  {
    for (it=0; it<nTria; it++)
    {
      i = tria[it].node[n];              // i, j define face of a triangle
      if (n < 2)
        j = tria[it].node[n+1];
      else
        j = tria[it].node[0];
      if (i > j)  // lower index first
      {
        d = i; i = j; j = d;
      }

      if (marker[i] && marker[j])        // must be on boundary
      {
        for (ibf=0; ibf<nBfaces; ibf++)  // search thru all boundary faces
        {
          if (btria[ibf] < 0)            // triangle unknown
          {
            n1 = bface[ibf].node1;       // n1, n2 define boundary face
            n2 = bface[ibf].node2;
            if (n1 > n2)                 // lower index first
            {
              d  = n1; n1 = n2; n2 = d;
            }
            if (i==n1 && j==n2) btria[ibf] = it; // triangle found
          }
        }
      }
    } // loop over triangles
  }   // loop over triangle nodes

  // check if all pointers from boundary faces to triangles valid

  for (ibf=0; ibf<nBfaces; ibf++)
  {
    if (btria[ibf] < 0) throw runtime_error( "invalid pointer from boundary face to triangle." );
  }

  // compute boundary face vector sbf

  for (ibf=0; ibf<nBfaces; ibf++)
  {
    n1 = bface[ibf].node1;
    n2 = bface[ibf].node2;
    sbf[ibf].x = coords[n2].y - coords[n1].y;
    sbf[ibf].y = coords[n1].x - coords[n2].x;

    // change orientation of sbf (must be outward facing)
    it    = btria[ibf];
    nt1   = tria[it].node[0];
    nt2   = tria[it].node[1];
    nt3   = tria[it].node[2];
    cx    = (coords[nt1].x+coords[nt2].x+coords[nt3].x)/3.0;
    cy    = (coords[nt1].y+coords[nt2].y+coords[nt3].y)/3.0;
    xm    = 0.5*(coords[n1].x+coords[n2].x);
    ym    = 0.5*(coords[n1].y+coords[n2].y);
    vprod = sbf[ibf].x*(xm-cx) + sbf[ibf].y*(ym-cy);
    if (vprod < 0.0)
    {
      sbf[ibf].x = -sbf[ibf].x;
      sbf[ibf].y = -sbf[ibf].y;
    }
  }
  
  // release temporary storage

  delete[] marker;
  delete[] btria;

  // generate face vectors for dummy edges ************************************

  ibegf = 0;
  ibegn = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendf = ibound[ib].bfaceIndex;
    iendn = ibound[ib].bnodeIndex;
    itype = btype[ib];
    flag  = false;
    if (itype>=100 && itype<200) flag = true;
    if (itype>=200 && itype<300) flag = true;
    if (itype>=600 && itype<700) flag = true;
    if (flag)                               // inlet/outlet/far-field boundary
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)    // loop over boundary faces
      {
        n1 = bface[ibf].node1;
        n2 = bface[ibf].node2;
        for (ibn=ibegn; ibn<=iendn; ibn++)  // search for corresp. boundary nodes
        {
          if (bnode[ibn].node == n1)
          {
            ie = bnode[ibn].edge;
            n1 = -777;
            sij[ie].x += 0.5*sbf[ibf].x;
            sij[ie].y += 0.5*sbf[ibf].y;
          }
          else if (bnode[ibn].node == n2)
          {
            ie = bnode[ibn].edge;
            n2 = -777;
            sij[ie].x += 0.5*sbf[ibf].x;
            sij[ie].y += 0.5*sbf[ibf].y;
          }
          if (n1<0 && n2<0) break;
        }
      } // ibf
    }   // flag
    ibegf = iendf + 1;
    ibegn = iendn + 1;
  } // ib

  // set coordinates and volumes of dummy nodes *******************************

  for (ie=nedInt; ie<nEdges; ie++)
  {
    i = edge[ie].i;   // boundary node
    j = edge[ie].j;   // dummy node
    coords[j].x = coords[i].x;
    coords[j].y = coords[i].y;
    vol[j]      = vol[i];
  }
}