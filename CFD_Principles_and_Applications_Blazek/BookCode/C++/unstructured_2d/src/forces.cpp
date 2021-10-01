/// @file forces.cpp
///
/// Computation of forces and moments.
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

/// Computes pressure forces and moments acting on the body. The contributions
/// are summed up by looping over ALL walls. Function also computes lift and
/// drag coefficients.
///
void Solver::Forces()
{
  int  ib, ibf, ibegf, iendf, n1, n2;
  REAL sx, sy, pwall, cp, xa, ya, dcx, dcy, cx, cy;

  DEPVARS *dv  = fluidProps.dv;
  NODE *coords = geometry.coords;
  NODE *sbf    = geometry.sbf;

  // initialize force coefficients

  cx = 0.0;
  cy = 0.0;
  cm = 0.0;

  // loop over boundaries searching for walls

  ibegf = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendf = geometry.ibound[ib].bfaceIndex;

    if (geometry.btype[ib]>=300 && geometry.btype[ib]<500)
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        n1    = geometry.bface[ibf].node1;
        n2    = geometry.bface[ibf].node2;
        sx    = sbf[ibf].x;
        sy    = sbf[ibf].y;
        pwall = 0.5*(dv[n1].press+dv[n2].press);
        cp    = 2.0*(pwall-bndConds.pinf)/(bndConds.rhoinf*bndConds.qinf*bndConds.qinf);
        xa    = (0.5*(coords[n1].x+coords[n2].x)-geometry.xref)/geometry.cref;
        ya    = (0.5*(coords[n1].y+coords[n2].y)-geometry.yref)/geometry.cref;
        dcy   = sy*cp;
        dcx   = sx*cp;
        cy    = cy + dcy;
        cx    = cx + dcx;
        cm    = cm + dcx*ya - dcy*xa;
      }
    }
    ibegf = iendf + 1;
  }

  // final lift and drag coefficients (pressure forces only!)

  cl = cy*COS(bndConds.alpha) - cx*SIN(bndConds.alpha);
  cd = cy*SIN(bndConds.alpha) + cx*COS(bndConds.alpha);
}