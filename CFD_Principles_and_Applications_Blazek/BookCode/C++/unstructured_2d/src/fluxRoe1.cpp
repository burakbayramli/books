/// @file fluxRoe1.cpp
///
/// Computation of the convective fluxes based on flux averages.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 7, 2014
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

/// Computes the convective fluxes using average of fluxes at control volume's
/// faces. The left and right fluxes are computed directly from values at nodes
/// "i" and "j". This is sufficient for a 1st-order Roe scheme.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
///
void SpaceDiscr::FluxRoe1( const Geometry &geometry, const FluidProps &fluidProps )
{
  int  i, j, ie;
  REAL rhl, rhr, qsl, qsr, pav;
  REAL fc[4];

  CONSVARS       *cv   = fluidProps.cv;
  DEPVARS        *dv   = fluidProps.dv;
  NODE           *sij  = geometry.sij;
  Geometry::EDGE *edge = geometry.edge;

  // initialize residual by adding artificial dissipation

  for (i=0; i<geometry.nNodes; i++)
  {
    rhs[i].dens = -diss[i].dens;
    rhs[i].xmom = -diss[i].xmom;
    rhs[i].ymom = -diss[i].ymom;
    rhs[i].ener = -diss[i].ener;
  }

  // average of fluxes

  for (ie=0; ie<geometry.nEdges; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;

    // left and right rho*H, V*n

    rhl = dv[i].press + cv[i].ener;
    qsl = (cv[i].xmom*sij[ie].x+cv[i].ymom*sij[ie].y)/cv[i].dens;

    rhr = dv[j].press + cv[j].ener;
    qsr = (cv[j].xmom*sij[ie].x+cv[j].ymom*sij[ie].y)/cv[j].dens;

    // fluxes

    pav   = 0.5*(dv[i].press+dv[j].press);
    fc[0] = 0.5*(qsl*cv[i].dens+qsr*cv[j].dens);
    fc[1] = 0.5*(qsl*cv[i].xmom+qsr*cv[j].xmom) + sij[ie].x*pav;
    fc[2] = 0.5*(qsl*cv[i].ymom+qsr*cv[j].ymom) + sij[ie].y*pav;
    fc[3] = 0.5*(qsl*rhl       +qsr*rhr);

    rhs[i].dens += fc[0];
    rhs[i].xmom += fc[1];
    rhs[i].ymom += fc[2];
    rhs[i].ener += fc[3];

    rhs[j].dens -= fc[0];
    rhs[j].xmom -= fc[1];
    rhs[j].ymom -= fc[2];
    rhs[j].ener -= fc[3];
  }

  // treatment of solid walls

  FluxWalls( geometry,fluidProps );
}