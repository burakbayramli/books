/// @file bcondInflow.cpp
///
/// Treatment of inflow boundaries.
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

/// Applies inflow boundary condition at dummy points (subsonic flow assumed).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param ibegn       indirect pointer to first node of the boundary
/// @param iendn       indirect pointer to last node of the boundary
///
void BndConds::BcondInflow( const Geometry &geometry, FluidProps &fluidProps,
                            int ibegn, int iendn ) const
{
  int  ib, ibn, idn, ie;
  REAL ds, sxn, syn, rrho, u, v, uabs, unorm, cosa, c02, rinv, dis,
       cb, cc02, tb, pb, rhob, uabsb, ub, vb, gam1, ggm1, rgas;

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
    ggm1  = dv[ibn].gamma/gam1;
    rgas  = gam1*dv[ibn].cpgas/dv[ibn].gamma;
    rrho  = 1.0/cv[ibn].dens;
    u     = cv[ibn].xmom*rrho;
    v     = cv[ibn].ymom*rrho;
    uabs  = SQRT(u*u+v*v);
    unorm = u*sxn + v*syn;
    if (uabs < EPSGLO)
      cosa = 1.0;
    else
      cosa = -unorm/uabs;

    c02   = dv[ibn].csoun*dv[ibn].csoun + 0.5*gam1*uabs*uabs;
    rinv  = unorm - 2.0*dv[ibn].csoun/gam1;
    dis   = (gam1*cosa*cosa+2.0)*c02/(gam1*rinv*rinv) - 0.5*gam1;
    dis   = MAX(dis,EPSGLO);
    cb    = -rinv*(gam1/(gam1*cosa*cosa+2.0))*(1.0+cosa*SQRT(dis));
    cc02  = MIN((cb*cb)/c02,1.0);
    tb    = ttinl*cc02;
    pb    = ptinl*POW(tb/ttinl,ggm1);
    rhob  = pb/(rgas*tb);
    uabsb = (2.0*dv[ibn].cpgas)*(ttinl-tb);
    uabsb = SQRT(uabsb);
    ub    = uabsb*COS(betainl);
    vb    = uabsb*SIN(betainl);

    cv[idn].dens = rhob;
    cv[idn].xmom = rhob*ub;
    cv[idn].ymom = rhob*vb;
    cv[idn].ener = pb/gam1 + 0.5*rhob*(ub*ub+vb*vb);

    fluidProps.DependentVarsOne( idn );
  } // ib
}