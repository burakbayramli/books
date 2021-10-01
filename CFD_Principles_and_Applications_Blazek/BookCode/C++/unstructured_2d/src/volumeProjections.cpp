/// @file volumeProjections.cpp
///
/// Computation of control volume projections.
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

#include "defs.h"
#include "geometry.h"

/// Computes projections of the control volumes on the x- and y-axis.
/// Variable (sproj) is used later to compute the time step.
///
void Geometry::VolumeProjections()
{
  int  ibegn, iendn, ibegf, iendf;
  int  i, j, ib, ibf, ibn, ie;
  REAL sx, sy;

  // zero out the variable

  for (i=0; i<nNodes; i++)
  {
    sproj[i].x = 0.0;
    sproj[i].y = 0.0;
  }

  // sum up contributions from interior edges

  for (ie=0; ie<nedInt; ie++)
  {
    i  = edge[ie].i;
    j  = edge[ie].j;
    sx = 0.5*ABS(sij[ie].x);
    sy = 0.5*ABS(sij[ie].y);
    sproj[i].x += sx;
    sproj[i].y += sy;
    sproj[j].x += sx;
    sproj[j].y += sy;
  }

  // add contributions from boundaries (except periodic ones)

  ibegf = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendf = ibound[ib].bfaceIndex;
    if (btype[ib]<700 || btype[ib]>=800)
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        i  = bface[ibf].node1;
        j  = bface[ibf].node2;
        sx = 0.25*ABS(sbf[ibf].x);
        sy = 0.25*ABS(sbf[ibf].y);
        sproj[i].x += sx;
        sproj[i].y += sy;
        sproj[j].x += sx;
        sproj[j].y += sy;
      }
    }
    ibegf = iendf + 1;
  }

  // sum up at periodic boundaries

  ibegn = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendn = ibound[ib].bnodeIndex;
    if (btype[ib]>=700 && btype[ib]<800)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = bnode[ibn].node;
        j = bnode[ibn].dummy;
        sproj[i].x += sproj[j].x;
        sproj[i].y += sproj[j].y;
        sproj[j].x  = sproj[i].x;
        sproj[j].y  = sproj[i].y;
      }
    }
    ibegn = iendn + 1;
  }
}