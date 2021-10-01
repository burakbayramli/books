/// @file limiter.cpp
///
/// Computation of limiter functions.
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

/// Evaluates limiter functions (Venkatakrishnan's limiter, Eq. (5.67)).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
///
void SpaceDiscr::Limiter( const Geometry &geometry, const FluidProps &fluidProps )
{
  int  i, j, ib, ibn, ie, ibegn, iendn;
  REAL limfac3, rvolref, rx, ry, voll, eps2nl, d1minl, d1maxl, d2l,
       ul, vl, volr, eps2nr, d1minr, d1maxr, d2r, ur, vr, limval;
  REAL eps2[4];

  CONSVARS       *cv     = fluidProps.cv;
  DEPVARS        *dv     = fluidProps.dv;
  NODE           *coords = geometry.coords;
  REAL           *vol    = geometry.vol;
  Geometry::EDGE *edge   = geometry.edge;

  // initialize limiter functions (1.0 = no limiting)

  for (i=0; i<geometry.nNodes; i++)
  {
    lim[i].dens  = 1.0;
    lim[i].uvel  = 1.0;
    lim[i].vvel  = 1.0;
    lim[i].press = 1.0;
  }

  // normalize epsilon^2 for all limited variables (rho, u, v, p)

  limfac3 = limfac*limfac*limfac;
  rvolref = 1.0/POW(volRef,1.5);
  eps2[0] = (limfac3*rvolref)*(limRef.dens *limRef.dens );
  eps2[1] = (limfac3*rvolref)*(limRef.uvel *limRef.uvel );
  eps2[2] = (limfac3*rvolref)*(limRef.vvel *limRef.vvel );
  eps2[3] = (limfac3*rvolref)*(limRef.press*limRef.press);

  // evaluate limiter functions ***********************************************

  for (ie=0; ie<geometry.nedInt; ie++)
  {
    i = edge[ie].i;
    j = edge[ie].j;

    rx   = 0.5*(coords[j].x-coords[i].x);
    ry   = 0.5*(coords[j].y-coords[i].y);
    voll = POW(vol[i],1.5);
    volr = POW(vol[j],1.5);

    // density

    eps2nl      = eps2[0]*voll;
    d1minl      = umin[i].dens - cv[i].dens;
    d1maxl      = umax[i].dens - cv[i].dens;
    eps2nr      = eps2[0]*volr;
    d1minr      = umin[j].dens - cv[j].dens;
    d1maxr      = umax[j].dens - cv[j].dens;
    d2l         =  gradx[i].dens*rx + grady[i].dens*ry;
    d2r         = -gradx[j].dens*rx - grady[j].dens*ry;
    limval      = Venkat( d2l,d1minl,d1maxl,eps2nl );
    lim[i].dens = MIN(limval,lim[i].dens);
    limval      = Venkat( d2r,d1minr,d1maxr,eps2nr );
    lim[j].dens = MIN(limval,lim[j].dens);

    // u

    ul          = cv[i].xmom/cv[i].dens;
    ur          = cv[j].xmom/cv[j].dens;
    eps2nl      = eps2[1]*voll;
    d1minl      = umin[i].uvel - ul;
    d1maxl      = umax[i].uvel - ul;
    eps2nr      = eps2[1]*volr;
    d1minr      = umin[j].uvel - ur;
    d1maxr      = umax[j].uvel - ur;
    d2l         =  gradx[i].uvel*rx + grady[i].uvel*ry;
    d2r         = -gradx[j].uvel*rx - grady[j].uvel*ry;
    limval      = Venkat( d2l,d1minl,d1maxl,eps2nl );
    lim[i].uvel = MIN(limval,lim[i].uvel);
    limval      = Venkat( d2r,d1minr,d1maxr,eps2nr );
    lim[j].uvel = MIN(limval,lim[j].uvel);

    // v

    vl          = cv[i].ymom/cv[i].dens;
    vr          = cv[j].ymom/cv[j].dens;
    eps2nl      = eps2[2]*voll;
    d1minl      = umin[i].vvel - vl;
    d1maxl      = umax[i].vvel - vl;
    eps2nr      = eps2[2]*volr;
    d1minr      = umin[j].vvel - vr;
    d1maxr      = umax[j].vvel - vr;
    d2l         =  gradx[i].vvel*rx + grady[i].vvel*ry;
    d2r         = -gradx[j].vvel*rx - grady[j].vvel*ry;
    limval      = Venkat( d2l,d1minl,d1maxl,eps2nl );
    lim[i].vvel = MIN(limval,lim[i].vvel);
    limval      = Venkat( d2r,d1minr,d1maxr,eps2nr );
    lim[j].vvel = MIN(limval,lim[j].vvel);

    // pressure

    eps2nl       = eps2[3]*voll;
    d1minl       = umin[i].press - dv[i].press;
    d1maxl       = umax[i].press - dv[i].press;
    eps2nr       = eps2[3]*volr;
    d1minr       = umin[j].press - dv[j].press;
    d1maxr       = umax[j].press - dv[j].press;
    d2l          =  gradx[i].press*rx + grady[i].press*ry;
    d2r          = -gradx[j].press*rx - grady[j].press*ry;
    limval       = Venkat( d2l,d1minl,d1maxl,eps2nl );
    lim[i].press = MIN(limval,lim[i].press);
    limval       = Venkat( d2r,d1minr,d1maxr,eps2nr );
    lim[j].press = MIN(limval,lim[j].press);
  }

  // periodic boundaries ******************************************************

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

        lim[i].dens  = MIN(lim[i].dens ,lim[j].dens );
        lim[i].uvel  = MIN(lim[i].uvel ,lim[j].uvel );
        lim[i].vvel  = MIN(lim[i].vvel ,lim[j].vvel );
        lim[i].press = MIN(lim[i].press,lim[j].press);

        lim[j].dens  = lim[i].dens;
        lim[j].uvel  = lim[i].uvel;
        lim[j].vvel  = lim[i].vvel;
        lim[j].press = lim[i].press;
      }
    }
    ibegn = iendn + 1;
  }
}