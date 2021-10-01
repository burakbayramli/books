/// @file gradients.cpp
///
/// Computation of gradients (rho, u, v, p) at the grid nodes.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 17, 2014
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

#include "bndConds.h"
#include "spaceDiscr.h"

/// Computes gradients of the density, u, v, and of the pressure with respect
/// to the x- and y-coordinates. Gradients are evaluated at the grid nodes.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
///
void SpaceDiscr::Gradients( const Geometry &geometry, const FluidProps &fluidProps )
{
  bool flag;
  int  i, j, ib, ibf, ibn, ie, ibegf, iendf, ibegn, iendn;
  REAL rav, uav, vav, pav, sx, sy;
  REAL fcx[4], fcy[4];

  CONSVARS       *cv   = fluidProps.cv;
  DEPVARS        *dv   = fluidProps.dv;
  NODE           *sij  = geometry.sij;
  REAL           *vol  = geometry.vol;
  Geometry::EDGE *edge = geometry.edge;

  // initialize gradients to zero

  for (i=0; i<geometry.nNodes; i++)
  {
    gradx[i].dens  = 0.0;
    gradx[i].uvel  = 0.0;
    gradx[i].vvel  = 0.0;
    gradx[i].press = 0.0;

    grady[i].dens  = 0.0;
    grady[i].uvel  = 0.0;
    grady[i].vvel  = 0.0;
    grady[i].press = 0.0;
  }

  // sum up contributions at edge endpoints ***********************************

  for (ie=0; ie<geometry.nedInt; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;

    // average of variables

    rav = 0.5*(cv[i].dens+cv[j].dens);
    uav = 0.5*(cv[i].xmom/cv[i].dens+cv[j].xmom/cv[j].dens);
    vav = 0.5*(cv[i].ymom/cv[i].dens+cv[j].ymom/cv[j].dens);
    pav = 0.5*(dv[i].press+dv[j].press);

    // gradients (later divided by the volume)

    sx = sij[ie].x;
    sy = sij[ie].y;

    fcx[0] = rav*sx;
    fcx[1] = uav*sx;
    fcx[2] = vav*sx;
    fcx[3] = pav*sx;

    fcy[0] = rav*sy;
    fcy[1] = uav*sy;
    fcy[2] = vav*sy;
    fcy[3] = pav*sy;

    gradx[i].dens  += fcx[0];
    gradx[i].uvel  += fcx[1];
    gradx[i].vvel  += fcx[2];
    gradx[i].press += fcx[3];
    gradx[j].dens  -= fcx[0];
    gradx[j].uvel  -= fcx[1];
    gradx[j].vvel  -= fcx[2];
    gradx[j].press -= fcx[3];

    grady[i].dens  += fcy[0];
    grady[i].uvel  += fcy[1];
    grady[i].vvel  += fcy[2];
    grady[i].press += fcy[3];
    grady[j].dens  -= fcy[0];
    grady[j].uvel  -= fcy[1];
    grady[j].vvel  -= fcy[2];
    grady[j].press -= fcy[3];
  }

  // contributions from the boundaries ****************************************

  ibegf = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendf = geometry.ibound[ib].bfaceIndex;
    flag  = true;
    if (geometry.btype[ib]>=500 && geometry.btype[ib]<600) flag = false;
    if (geometry.btype[ib]>=700 && geometry.btype[ib]<800) flag = false;

    if (flag)            // all except symmetry and periodic boundaries
    {
      for (ibf=ibegf; ibf<=iendf; ibf++)
      {
        i  = geometry.bface[ibf].node1;
        j  = geometry.bface[ibf].node2;
        sx = geometry.sbf[ibf].x/12.0;
        sy = geometry.sbf[ibf].y/12.0;

        // node i

        rav = 5.0*cv[i].dens            + cv[j].dens;
        uav = 5.0*cv[i].xmom/cv[i].dens + cv[j].xmom/cv[j].dens;
        vav = 5.0*cv[i].ymom/cv[i].dens + cv[j].ymom/cv[j].dens;
        pav = 5.0*dv[i].press           + dv[j].press;

        gradx[i].dens  += rav*sx;
        gradx[i].uvel  += uav*sx;
        gradx[i].vvel  += vav*sx;
        gradx[i].press += pav*sx;

        grady[i].dens  += rav*sy;
        grady[i].uvel  += uav*sy;
        grady[i].vvel  += vav*sy;
        grady[i].press += pav*sy;

        // node j

        rav = 5.0*cv[j].dens            + cv[i].dens;
        uav = 5.0*cv[j].xmom/cv[j].dens + cv[i].xmom/cv[i].dens;
        vav = 5.0*cv[j].ymom/cv[j].dens + cv[i].ymom/cv[i].dens;
        pav = 5.0*dv[j].press           + dv[i].press;

        gradx[j].dens  += rav*sx;
        gradx[j].uvel  += uav*sx;
        gradx[j].vvel  += vav*sx;
        gradx[j].press += pav*sx;

        grady[j].dens  += rav*sy;
        grady[j].uvel  += uav*sy;
        grady[j].vvel  += vav*sy;
        grady[j].press += pav*sy;
      }
    }
    ibegf = iendf + 1;
  }

  // correct at symmetry boundaries

  ibegn = 0;

  for (ib=0; ib<geometry.nSegs; ib++)
  {
    iendn = geometry.ibound[ib].bnodeIndex;

    if (geometry.btype[ib]>=500 && geometry.btype[ib]<600)
    {
      if ((geometry.btype[ib]-500) < 2)  // x=const. line
      {
        for (ibn=ibegn; ibn<=iendn; ibn++)
        {
          i = geometry.bnode[ibn].node;
          gradx[i].dens  = 0.0;
          grady[i].uvel  = 0.0;
          gradx[i].vvel  = 0.0;
          gradx[i].press = 0.0;
        }
      }
      else                               // y=const. line
      {
        for (ibn=ibegn; ibn<=iendn; ibn++)
        {
          i = geometry.bnode[ibn].node;
          grady[i].dens  = 0.0;
          grady[i].uvel  = 0.0;
          gradx[i].vvel  = 0.0;
          grady[i].press = 0.0;
        }
      }
    }
    ibegn = iendn + 1;
  }

  // sum up at periodic boundaries

  BndConds::Periodic( geometry,gradx );
  BndConds::Periodic( geometry,grady );

  // divide by the control volume *********************************************

  for (i=0; i<geometry.nndInt; i++)
  {
    gradx[i].dens  /= vol[i];
    gradx[i].uvel  /= vol[i];
    gradx[i].vvel  /= vol[i];
    gradx[i].press /= vol[i];

    grady[i].dens  /= vol[i];
    grady[i].uvel  /= vol[i];
    grady[i].vvel  /= vol[i];
    grady[i].press /= vol[i];
  }
}