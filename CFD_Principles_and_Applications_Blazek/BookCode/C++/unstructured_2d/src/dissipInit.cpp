/// @file dissipInit.cpp
///
/// Initialization of artificial dissipation.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 5, 2014
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

#include "spaceDiscr.h"

/// Initializes the artificial dissipation vector at all grid nodes.
///
/// @param irk     stage of the Runge-Kutta scheme
/// @param nNodes  total number of grid nodes
/// @param beta    dissipation-blending coefficient
///
void SpaceDiscr::DissipInit( int irk, int nNodes, REAL beta )
{
  int  i;
  REAL blend;

  if (irk==0 || beta>0.99)
  {
    for (i=0; i<nNodes; i++)
    {
      diss[i].dens = 0.0;
      diss[i].xmom = 0.0;
      diss[i].ymom = 0.0;
      diss[i].ener = 0.0;
    }
  }
  else
  {
    blend = 1.0 - beta;
    for (i=0; i<nNodes; i++)
    {
      diss[i].dens *= blend;
      diss[i].xmom *= blend;
      diss[i].ymom *= blend;
      diss[i].ener *= blend;
    }
  }
}