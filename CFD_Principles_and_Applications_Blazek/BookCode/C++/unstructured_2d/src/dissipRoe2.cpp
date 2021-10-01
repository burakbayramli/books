/// @file dissipRoe2.cpp
///
/// Computation of 2nd-order upwind dissipation.
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

/// Computes upwind dissipation according to 2nd-order Roe's flux-difference
/// splitting scheme. Values are extrapolated to the dual faces using Venkat's
/// limiter function.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param beta        dissipation-blending coefficient
///
void SpaceDiscr::DissipRoe2( const Geometry &geometry, const FluidProps &fluidProps,
                             REAL beta )
{
  int  i, j, ie;
  REAL beta5, ds, nx, ny, rx, ry, gam1, ggm1, rrho, rl, ul, vl, pl, hl, rr, ur, vr,
       pr, hr, rav, dd, dd1, uav, vav, hav, q2a, c2a, cav, uv, du, h1, h2, h3, h4,
       h5, delta, eabs1, eabs2, eabs4;
  REAL fd[4];

  CONSVARS       *cv     = fluidProps.cv;
  DEPVARS        *dv     = fluidProps.dv;
  NODE           *sij    = geometry.sij;
  NODE           *coords = geometry.coords;
  Geometry::EDGE *edge   = geometry.edge;

  beta5 = 0.5*beta;

  for (ie=0; ie<geometry.nEdges; ie++)
  {
    i  = edge[ie].i;
    j  = edge[ie].j;
    ds = SQRT(sij[ie].x*sij[ie].x+sij[ie].y*sij[ie].y);
    nx = sij[ie].x/ds;
    ny = sij[ie].y/ds;
    rx = 0.5*(coords[j].x-coords[i].x);
    ry = 0.5*(coords[j].y-coords[i].y);

    // left & right state

    rrho = 1.0/cv[i].dens;
    gam1 = dv[i].gamma - 1.0;
    ggm1 = dv[i].gamma/gam1;
    rl   = cv[i].dens      + lim[i].dens *(gradx[i].dens *rx+grady[i].dens *ry);
    ul   = cv[i].xmom*rrho + lim[i].uvel *(gradx[i].uvel *rx+grady[i].uvel *ry);
    vl   = cv[i].ymom*rrho + lim[i].vvel *(gradx[i].vvel *rx+grady[i].vvel *ry);
    pl   = dv[i].press     + lim[i].press*(gradx[i].press*rx+grady[i].press*ry);
    hl   = ggm1*pl/rl + 0.5*(ul*ul+vl*vl);

    rrho = 1.0/cv[j].dens;
    gam1 = dv[j].gamma - 1.0;
    ggm1 = dv[j].gamma/gam1;
    rr   = cv[j].dens      - lim[j].dens *(gradx[j].dens *rx+grady[j].dens *ry);
    ur   = cv[j].xmom*rrho - lim[j].uvel *(gradx[j].uvel *rx+grady[j].uvel *ry);
    vr   = cv[j].ymom*rrho - lim[j].vvel *(gradx[j].vvel *rx+grady[j].vvel *ry);
    pr   = dv[j].press     - lim[j].press*(gradx[j].press*rx+grady[j].press*ry);
    hr   = ggm1*pr/rr + 0.5*(ur*ur+vr*vr);

    // Roe's average

    rav  = SQRT(rl*rr);
    gam1 = 0.5*(dv[i].gamma+dv[j].gamma) - 1.0;
    dd   = rav/rl;
    dd1  = 1.0/(1.0+dd);
    uav  = (ul+dd*ur)*dd1;
    vav  = (vl+dd*vr)*dd1;
    hav  = (hl+dd*hr)*dd1;
    q2a  = 0.5*(uav*uav+vav*vav);
    c2a  = gam1*(hav-q2a);
    cav  = SQRT(c2a);
    uv   = uav*nx + vav*ny;
    du   = (ur-ul)*nx + (vr-vl)*ny;

    // eigenvalues

    h1    = ABS(uv - cav);
    h2    = ABS(uv);
    h4    = ABS(uv + cav);
    delta = epsEntr*h4;

    eabs1 = EntropyCorr( h1,delta );
    eabs2 = EntropyCorr( h2,delta );
    eabs4 = EntropyCorr( h4,delta );

    // upwind fluxes

    h1 = rav*cav*du;
    h2 = eabs1*(pr-pl - h1)/(2.0*c2a);
    h3 = eabs2*(rr-rl - (pr-pl)/c2a);
    h4 = eabs2*rav;
    h5 = eabs4*(pr-pl + h1)/(2.0*c2a);

    fd[0] = h2 + h3 + h5;
    fd[1] = h2*(uav-cav*nx) + h3*uav + h4*(ur-ul-du*nx) + h5*(uav+cav*nx);
    fd[2] = h2*(vav-cav*ny) + h3*vav + h4*(vr-vl-du*ny) + h5*(vav+cav*ny);
    fd[3] = h2*(hav-cav*uv) + h3*q2a + h4*(uav*(ur-ul)+vav*(vr-vl)-uv*du) +
            h5*(hav+cav*uv);

    // edge contributions to dissipation

    ds           *= beta5;
    diss[i].dens += fd[0]*ds;
    diss[i].xmom += fd[1]*ds;
    diss[i].ymom += fd[2]*ds;
    diss[i].ener += fd[3]*ds;

    diss[j].dens -= fd[0]*ds;
    diss[j].xmom -= fd[1]*ds;
    diss[j].ymom -= fd[2]*ds;
    diss[j].ener -= fd[3]*ds;
  }
}