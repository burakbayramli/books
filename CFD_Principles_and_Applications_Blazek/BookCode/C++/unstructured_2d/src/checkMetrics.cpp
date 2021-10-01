/// @file checkMetrics.cpp
///
/// Checking of grid metrics for correctness.
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

#include <iostream>
#include "geometry.h"

using namespace std;

/// Checks metrics by computing min. and max. volume, and by computing
/// the sum of face vectors for each control volume (should be zero).
///
void Geometry::CheckMetrics()
{
  int  i, j, ib, ibf, ibn, ie, ibegf, iendf, ibegn, iendn;
  REAL volmin, volmax, s, smax;
  NODE *fvecSum;

  // obtain mix. and max. control volume

  volmin = +1.0e+32;
  volmax = -1.0e+32;
  for (i=0; i<nndInt; i++)
  {
    volmin = MIN(volmin,vol[i]);
    volmax = MAX(volmax,vol[i]);
  }

  // compute sum of face vectors for each control volume (fvecSum)

  fvecSum = new NODE[nndInt];
  for (i=0; i<nndInt; i++)
  {
    fvecSum[i].x = 0.0;
    fvecSum[i].y = 0.0;
  }

  for (ie=0; ie<nedInt; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;
    fvecSum[i].x += sij[ie].x;
    fvecSum[i].y += sij[ie].y;
    fvecSum[j].x -= sij[ie].x;
    fvecSum[j].y -= sij[ie].y;
  }

  ibegf = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendf = ibound[ib].bfaceIndex;
    if (btype[ib]<700 || btype[ib]>=800)  // boundary faces (non-periodic)
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        i = bface[ibf].node1;
        j = bface[ibf].node2;
        fvecSum[i].x += 0.5*sbf[ibf].x;
        fvecSum[i].y += 0.5*sbf[ibf].y;
        fvecSum[j].x += 0.5*sbf[ibf].x;
        fvecSum[j].y += 0.5*sbf[ibf].y;
      }
    }
    ibegf = iendf + 1;
  }

  ibegn = 0;
  for (ib=0; ib<nSegs; ib++)
  {
    iendn = ibound[ib].bnodeIndex;
    if (btype[ib]>=700 && btype[ib]<800)  // periodic nodes
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = bnode[ibn].node;
        j = bnode[ibn].dummy;
        fvecSum[i].x += fvecSum[j].x;
        fvecSum[i].y += fvecSum[j].y;
        fvecSum[j].x  = fvecSum[i].x;
        fvecSum[j].y  = fvecSum[i].y;
      }
    }
    ibegn = iendn + 1;
  }

  // compute maximum of the sum of face vectors

  smax = -1.0e+32;
  for (i=0; i<nndInt; i++)
  {
    s    = SQRT(fvecSum[i].x*fvecSum[i].x+fvecSum[i].y*fvecSum[i].y);
    smax = MAX(smax,s);
  }

  cout << " max. sum(S) = " << smax << endl;
  cout << " min. volume = " << volmin << endl;
  cout << " max. volume = " << volmax << endl << endl;

  delete[] fvecSum;
}