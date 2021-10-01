/// @file bcondOutflow.cpp
///
/// Treatment of outflow boundaries.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 2, 2014
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
#include "bndConds.h"

/// Applies outflow boundary condition to dummy points. Sub- or supersonic
/// outflow is possible.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param ibegn       indirect pointer to first node of the boundary
/// @param iendn       indirect pointer to last node of the boundary
///
void BndConds::BcondOutflow( const Geometry &geometry, FluidProps &fluidProps,
                             int ibegn, int iendn ) const
{
  int  ib, ibn, idn, ie;
  REAL gam1, ds, sxn, syn, rrho, u, v, rrhoc, deltp,
       rhob, ub, vb, vnd, q, mach;

  CONSVARS *cv  = fluidProps.cv;
  DEPVARS  *dv  = fluidProps.dv;
  NODE     *sij = geometry.sij;

  for (ib=ibegn; ib<=iendn; ib++)
  {
    ibn = geometry.bnode[ib].node;   // boundary node
    idn = geometry.bnode[ib].dummy;  // dummy node
    ie  = geometry.bnode[ib].edge;   // edge to dummy node

    ds  = SQRT(sij[ie].x*sij[ie].x+sij[ie].y*sij[ie].y);
    sxn = sij[ie].x/ds;
    syn = sij[ie].y/ds;

    gam1  = dv[ibn].gamma - 1.0;
    rrho  = 1.0/cv[ibn].dens;
    u     = cv[ibn].xmom*rrho;
    v     = cv[ibn].ymom*rrho;
    q     = SQRT(u*u+v*v);
    mach  = q/dv[ibn].csoun;

    if (mach < 1.0)
    {
      rrhoc = rrho/dv[ibn].csoun;
      deltp = dv[ibn].press - pout;
      rhob  = cv[ibn].dens - deltp/(dv[ibn].csoun*dv[ibn].csoun);
      ub    = u + sxn*deltp*rrhoc;
      vb    = v + syn*deltp*rrhoc;

      // special treatment to prevent "deltp" from changing the sign
      // of velocity components. This may happen for very small u, v.

      vnd = ub*sxn + vb*syn;
      if (vnd < 0.0)
      {
        ub = SIGN(1.0,u)*MAX(ABS(ub),ABS(u));
        vb = SIGN(1.0,v)*MAX(ABS(vb),ABS(v));
      }
      cv[idn].dens = rhob;
      cv[idn].xmom = rhob*ub;
      cv[idn].ymom = rhob*vb;
      cv[idn].ener = pout/gam1 + 0.5*rhob*(ub*ub+vb*vb);
    }
    else
    {
      cv[idn].dens = cv[ibn].dens;
      cv[idn].xmom = cv[ibn].xmom;
      cv[idn].ymom = cv[ibn].ymom;
      cv[idn].ener = cv[ibn].ener;
    }

    fluidProps.DependentVarsOne( idn );
  } // ib
}