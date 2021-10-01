/// @file fluxRoe2.cpp
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
/// faces. The left and right fluxes are computed from reconstructed and limited
/// values at nodes "i" and "j". This is required for a 2nd-order Roe scheme
/// (see Subsection 4.3.3).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
///
void SpaceDiscr::FluxRoe2( const Geometry &geometry, const FluidProps &fluidProps )
{
  int  i, j, ie;
  REAL rx, ry, rrho, gam1, ggm1, rl, ul, vl, pl, hl, rr, ur, vr, pr, hr,
       qsrl, qsrr, pav;
  REAL fc[4];

  CONSVARS       *cv     = fluidProps.cv;
  DEPVARS        *dv     = fluidProps.dv;
  NODE           *sij    = geometry.sij;
  NODE           *coords = geometry.coords;
  Geometry::EDGE *edge   = geometry.edge;

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
    i  = edge[ie].i;
    j  = edge[ie].j;
    rx = 0.5*(coords[j].x-coords[i].x);
    ry = 0.5*(coords[j].y-coords[i].y);

    // left and right state

    rrho = 1.0/cv[i].dens;
    gam1 = dv[i].gamma - 1.0;
    ggm1 = dv[i].gamma/gam1;
    rl   = cv[i].dens      + lim[i].dens *(gradx[i].dens *rx+grady[i].dens *ry);
    ul   = cv[i].xmom*rrho + lim[i].uvel *(gradx[i].uvel *rx+grady[i].uvel *ry);
    vl   = cv[i].ymom*rrho + lim[i].vvel *(gradx[i].vvel *rx+grady[i].vvel *ry);
    pl   = dv[i].press     + lim[i].press*(gradx[i].press*rx+grady[i].press*ry);
    hl   = ggm1*pl/rl + 0.5*(ul*ul+vl*vl);
    qsrl = (ul*sij[ie].x+vl*sij[ie].y)*rl;

    rrho = 1.0/cv[j].dens;
    gam1 = dv[j].gamma - 1.0;
    ggm1 = dv[j].gamma/gam1;
    rr   = cv[j].dens      - lim[j].dens *(gradx[j].dens *rx+grady[j].dens *ry);
    ur   = cv[j].xmom*rrho - lim[j].uvel *(gradx[j].uvel *rx+grady[j].uvel *ry);
    vr   = cv[j].ymom*rrho - lim[j].vvel *(gradx[j].vvel *rx+grady[j].vvel *ry);
    pr   = dv[j].press     - lim[j].press*(gradx[j].press*rx+grady[j].press*ry);
    hr   = ggm1*pr/rr + 0.5*(ur*ur+vr*vr);
    qsrr = (ur*sij[ie].x+vr*sij[ie].y)*rr;

    // fluxes

    pav   = 0.5*(pl+pr);
    fc[0] = 0.5*(qsrl   +qsrr   );
    fc[1] = 0.5*(qsrl*ul+qsrr*ur) + sij[ie].x*pav;
    fc[2] = 0.5*(qsrl*vl+qsrr*vr) + sij[ie].y*pav;
    fc[3] = 0.5*(qsrl*hl+qsrr*hr);

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