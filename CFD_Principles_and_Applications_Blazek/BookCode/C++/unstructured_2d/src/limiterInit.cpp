/// @file limiterInit.cpp
///
/// Computation of min./max. values around a node.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 11, 2014
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

/// Computes minimum and maximum values of density, u, v, and pressure for
/// all direct neighbors "j" of node "i" (min/max_j U_j in Eq. (5.61)). This
/// is used later in SpaceDiscr::Limiter to evaluate the limiter functions.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
///
void SpaceDiscr::LimiterInit( const Geometry &geometry, const FluidProps &fluidProps )
{
  int  i, j, ib, ibn, ie, ibegn, iendn;
  REAL rl, ul, vl, pl, rr, ur, vr, pr;

  CONSVARS       *cv   = fluidProps.cv;
  DEPVARS        *dv   = fluidProps.dv;
  Geometry::EDGE *edge = geometry.edge;

  // initialize with values at node i

  for (i=0; i<geometry.nndInt; i++)
  {
    umin[i].dens  = cv[i].dens;
    umin[i].uvel  = cv[i].xmom/cv[i].dens;
    umin[i].vvel  = cv[i].ymom/cv[i].dens;
    umin[i].press = dv[i].press;

    umax[i].dens  = umin[i].dens;
    umax[i].uvel  = umin[i].uvel;
    umax[i].vvel  = umin[i].vvel;
    umax[i].press = umin[i].press;
  }

  // loop over interior edges

  for (ie=0; ie<geometry.nedInt; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;

    // left state

    rl = cv[i].dens;
    ul = cv[i].xmom/rl;
    vl = cv[i].ymom/rl;
    pl = dv[i].press;

    // right state

    rr = cv[j].dens;
    ur = cv[j].xmom/rr;
    vr = cv[j].ymom/rr;
    pr = dv[j].press;

    // neighbors of node i

    umin[i].dens  = MIN(umin[i].dens ,rr);
    umin[i].uvel  = MIN(umin[i].uvel ,ur);
    umin[i].vvel  = MIN(umin[i].vvel ,vr);
    umin[i].press = MIN(umin[i].press,pr);

    umax[i].dens  = MAX(umax[i].dens ,rr);
    umax[i].uvel  = MAX(umax[i].uvel ,ur);
    umax[i].vvel  = MAX(umax[i].vvel ,vr);
    umax[i].press = MAX(umax[i].press,pr);

    // neighbors of node j

    umin[j].dens  = MIN(umin[j].dens ,rl);
    umin[j].uvel  = MIN(umin[j].uvel ,ul);
    umin[j].vvel  = MIN(umin[j].vvel ,vl);
    umin[j].press = MIN(umin[j].press,pl);

    umax[j].dens  = MAX(umax[j].dens ,rl);
    umax[j].uvel  = MAX(umax[j].uvel ,ul);
    umax[j].vvel  = MAX(umax[j].vvel ,vl);
    umax[j].press = MAX(umax[j].press,pl);
  }

  // periodic boundaries

  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendn = geometry.ibound[ib].bnodeIndex;

    if (geometry.btype[ib]>=700 && geometry.btype[ib]<800)
    {
      for (ibn=ibegn; ibn<=iendn; ibn++)
      {
        i = geometry.bnode[ibn].node;
        j = geometry.bnode[ibn].dummy;

        umin[i].dens  = MIN(umin[i].dens ,umin[j].dens );
        umin[i].uvel  = MIN(umin[i].uvel ,umin[j].uvel );
        umin[i].vvel  = MIN(umin[i].vvel ,umin[j].vvel );
        umin[i].press = MIN(umin[i].press,umin[j].press);

        umax[i].dens  = MAX(umax[i].dens ,umax[j].dens );
        umax[i].uvel  = MAX(umax[i].uvel ,umax[j].uvel );
        umax[i].vvel  = MAX(umax[i].vvel ,umax[j].vvel );
        umax[i].press = MAX(umax[i].press,umax[j].press);

        umin[j].dens  = umin[i].dens;
        umin[j].uvel  = umin[i].uvel;
        umin[j].vvel  = umin[i].vvel;
        umin[j].press = umin[i].press;

        umax[j].dens  = umax[i].dens;
        umax[j].uvel  = umax[i].uvel;
        umax[j].vvel  = umax[i].vvel;
        umax[j].press = umax[i].press;
      }
    }
    ibegn = iendn + 1;
  }
}