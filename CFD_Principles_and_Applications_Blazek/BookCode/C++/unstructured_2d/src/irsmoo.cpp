/// @file irsmoo.cpp
///
/// Central implicit residual smoothing.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 12, 2014
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

#include "timeDiscr.h"

/// Applies the central implicit smoothing method to the residuals
/// by using Jacobi iteration.
///
/// @param geometry    geometrical data
/// @param spaceDiscr  spatial discretization
///
void TimeDiscr::Irsmoo( const Geometry &geometry, SpaceDiscr &spaceDiscr )
{
  int  itirs, i, j, ie;
  REAL den;

  CONSVARS       *rhs  = spaceDiscr.rhs;
  Geometry::EDGE *edge = geometry.edge;

  // initialize counter (no. of contributions to a node);
  // store the non-smoothed residual in "rhsOld"

  for (i=0; i<geometry.nNodes; i++)
  {
    nContr[i] = 0;
    rhsOld[i] = rhs[i];
  }

  // Jacobi iteration *********************************************************

  for (itirs=1; itirs<=nIterIrs; itirs++)
  {
    // zero out nodal contributions

    for (i=0; i<geometry.nndInt; i++)
    {
      rhsIter[i].dens = 0.0;
      rhsIter[i].xmom = 0.0;
      rhsIter[i].ymom = 0.0;
      rhsIter[i].ener = 0.0;
    }

    // loop over edges: first iteration => set counter

    if (itirs == 1)
    {
      for (ie=0; ie<geometry.nedInt; ie++)
      {
        i = edge[ie].i;
        j = edge[ie].j;
        nContr[i]++;
        nContr[j]++;

        rhsIter[i].dens += rhs[j].dens;
        rhsIter[i].xmom += rhs[j].xmom;
        rhsIter[i].ymom += rhs[j].ymom;
        rhsIter[i].ener += rhs[j].ener;

        rhsIter[j].dens += rhs[i].dens;
        rhsIter[j].xmom += rhs[i].xmom;
        rhsIter[j].ymom += rhs[i].ymom;
        rhsIter[j].ener += rhs[i].ener;
      }

      // sum up no. of contributions at periodic nodes
      BndConds::Periodic( geometry,nContr );
    }

    // loop over edges: without the counter

    else
    {
      for (ie=0; ie<geometry.nedInt; ie++)
      {
        i = edge[ie].i;
        j = edge[ie].j;

        rhsIter[i].dens += rhs[j].dens;
        rhsIter[i].xmom += rhs[j].xmom;
        rhsIter[i].ymom += rhs[j].ymom;
        rhsIter[i].ener += rhs[j].ener;

        rhsIter[j].dens += rhs[i].dens;
        rhsIter[j].xmom += rhs[i].xmom;
        rhsIter[j].ymom += rhs[i].ymom;
        rhsIter[j].ener += rhs[i].ener;
      }
    }

    // periodic boundaries

    BndConds::Periodic( geometry,rhsIter );

    // new smoothed residual

    for (i=0; i<geometry.nndInt; i++)
    {
      den         = 1.0/(1.0+((REAL)nContr[i])*epsIrs);
      rhs[i].dens = (rhsIter[i].dens*epsIrs+rhsOld[i].dens)*den;
      rhs[i].xmom = (rhsIter[i].xmom*epsIrs+rhsOld[i].xmom)*den;
      rhs[i].ymom = (rhsIter[i].ymom*epsIrs+rhsOld[i].ymom)*den;
      rhs[i].ener = (rhsIter[i].ener*epsIrs+rhsOld[i].ener)*den;
    }
  } // itirs
}