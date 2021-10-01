/// @file massFlow.cpp
///
/// Computation of mass flow.
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

#include "solver.h"

/// Computes mass flow at inlet and mass flow ratio between inlet and outlet
/// boundaries. Contributions are summed up by looping over ALL inlet and
/// outlet boundaries.
///
void Solver::MassFlow()
{
  bool in, out;
  int  ibegf, iendf, n1, n2, ib, ibf;
  REAL sx, sy, massin, massout, mass;

  CONSVARS *cv = fluidProps.cv;
  NODE *sbf    = geometry.sbf;

  // initialize mass flow before summing up the contributions

  massin  = 0.0;
  massout = 0.0;
  mflow   = 0.0;
  mfratio = 0.0;

  in  = false;  // flow into the domain
  out = false;  // flow out of the domain

  // loop over boundaries searching for inlet / outlet

  ibegf = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendf = geometry.ibound[ib].bfaceIndex;

    if (geometry.btype[ib]>=100 && geometry.btype[ib]<300)
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        n1   = geometry.bface[ibf].node1;
        n2   = geometry.bface[ibf].node2;
        sx   = sbf[ibf].x;
        sy   = sbf[ibf].y;
        mass = 0.5*((cv[n1].xmom+cv[n2].xmom)*sx+(cv[n1].ymom+cv[n2].ymom)*sy);
        if (geometry.btype[ib]>=100 && geometry.btype[ib]<200)
        {
          massin  = massin - mass;   // inflow
          in = true;
        }
        else
        {
          massout = massout + mass;  // outflow
          out = true;
        }
      }
    }
    ibegf = iendf + 1;
  }

  // mass flow and ratio

  if (in)
  {
    mflow   = massin;
    mfratio = massout/massin;
  }
  if (!in && out)
  {
    mflow   = massout;
    mfratio = 1.0;
  }
}