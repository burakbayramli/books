/// @file bcondFarfield.cpp
///
/// Treatment of far-field boundaries.
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

/// Applies far-field boundary condition to dummy points. Characteristic
/// boundary conditions are employed in the case of subsonic flow. Conservative
/// variables are inter- / extrapolated in the case of supersonic flow. Vortex
/// correction is optionally applied to the flow variables (subsonic only).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param precond     low Mach-number preconditioning
/// @param ibegn       indirect pointer to first node of the boundary
/// @param iendn       indirect pointer to last node of the boundary
///
void BndConds::BcondFarfield( const Geometry &geometry, FluidProps &fluidProps,
                              const Precond &precond, int ibegn, int iendn )
{
  int  ib, ibn, ibval, idn, ie;
  REAL gmr, gmg, bet, cir, xa, ya, dist, angle, sn, dn, vc, qv2;
  REAL ds, sxn, syn, rhoe, ue, ve, qqe, pe, qn, crho0,
       rhoa, ua, va, pa, sgn, pb, dprhoc, gam1, ggm1;
  REAL rhop, rhoT, hT, theta, a1, ra1g, a4, a5, cs;

  CONSVARS *cv     = fluidProps.cv;
  DEPVARS  *dv     = fluidProps.dv;
  NODE     *sij    = geometry.sij;
  NODE     *coords = geometry.coords;

  // free-stream values (optionally corrected by a vortex) ********************

  if (vortCorr)  // values corrected
  {
    bet   = SQRT(1.0-machinf*machinf);
    cir   = 0.25*geometry.cref*cl*qinf/PI;
    ibval = 0;

    for (ib=ibegn; ib<=iendn; ib++)
    {
      ibn   = geometry.bnode[ib].node;   // boundary node
      gam1  = dv[ibn].gamma - 1.0;
      ggm1  = dv[ibn].gamma/gam1;
      gmr   = 1.0/dv[ibn].gamma;
      gmg   = gam1/dv[ibn].gamma;
      xa    = coords[ibn].x - geometry.xref;
      ya    = coords[ibn].y - geometry.yref;
      dist  = SQRT(xa*xa+ya*ya);
      angle = ATAN2(ya,xa);
      sn    = SIN(angle-alpha);
      dn    = 1.0 - machinf*machinf*sn*sn;
      vc    = cir*bet/(dn*dist);

      bndVals[ibval].uvel  = uinf + vc*SIN(angle);
      bndVals[ibval].vvel  = vinf - vc*COS(angle);
      qv2                  = bndVals[ibval].uvel*bndVals[ibval].uvel +
                             bndVals[ibval].vvel*bndVals[ibval].vvel;
      bndVals[ibval].press = POW(POW(pinf,gmg)+gmg*rhoinf*(qinf*qinf-qv2)/
                             (2.0*POW(pinf,gmr)),ggm1);
      bndVals[ibval].dens  = rhoinf*POW(bndVals[ibval].press/pinf,gmr);
      ibval++;
    } // ib

  }
  else  // values not corrected
  {
    ibval = 0;
    for (ib=ibegn; ib<=iendn; ib++)
    {
      bndVals[ibval].dens  = rhoinf;
      bndVals[ibval].uvel  = uinf;
      bndVals[ibval].vvel  = vinf;
      bndVals[ibval].press = pinf;
      ibval++;
    } // ib
  }

  // computation of the boundary values ***************************************

  ibval = 0;

  for (ib=ibegn; ib<=iendn; ib++)
  {
    ibn = geometry.bnode[ib].node;   // boundary node
    idn = geometry.bnode[ib].dummy;  // dummy node
    ie  = geometry.bnode[ib].edge;   // edge to dummy node

    ds  = SQRT(sij[ie].x*sij[ie].x+sij[ie].y*sij[ie].y);
    sxn = sij[ie].x/ds;
    syn = sij[ie].y/ds;

    gam1 = dv[ibn].gamma - 1.0;
    rhoe = cv[ibn].dens;
    ue   = cv[ibn].xmom/rhoe;
    ve   = cv[ibn].ymom/rhoe;
    pe   = dv[ibn].press;
    qn   = sxn*ue + syn*ve;

    if (machinf < 1.0)
    {
      // subsonic flow (qn<0: inflow / qn>0: outflow)

      if (precond.switchedOn)
      {
        qqe   = ue*ue + ve*ve;
        rhop  =  rhoe/pe;
        rhoT  = -rhoe/dv[ibn].temp;
        hT    = dv[ibn].cpgas;
        theta = precond.ComputeTheta( dv[ibn].gamma,dv[ibn].csoun,qqe );
        a1    = rhoe*rhop*hT + rhoT;
        ra1g  = 1.0/(rhoe*theta*hT + rhoT);
        a4    = a1*ra1g;
        a5    = rhoe*hT*ra1g;
        cs    = SQRT((qn*qn)*((a4-1.0)*(a4-1.0))+4.0*a5);
        crho0 = rhoe*cs;
      }
      else
        crho0 = dv[ibn].csoun*rhoe;

      if (qn < 0.0)
      {
        rhoa = bndVals[ibval].dens;
        ua   = bndVals[ibval].uvel;
        va   = bndVals[ibval].vvel;
        pa   = bndVals[ibval].press;
        sgn  = -1.0;
        pb   = 0.5*(pa+pe-crho0*(sxn*(ua-ue)+syn*(va-ve)));
      }
      else
      {
        rhoa = rhoe;
        ua   = ue;
        va   = ve;
        pa   = pe;
        sgn  = +1.0;
        pb   = bndVals[ibval].press;
      }
      dprhoc       = sgn*(pa-pb)/crho0;
      cv[idn].dens = rhoa + (pb-pa)/(dv[ibn].csoun*dv[ibn].csoun);
      cv[idn].xmom = cv[idn].dens*(ua+sxn*dprhoc);
      cv[idn].ymom = cv[idn].dens*(va+syn*dprhoc);
      cv[idn].ener = pb/gam1 + 0.5*(cv[idn].xmom*cv[idn].xmom+
                                    cv[idn].ymom*cv[idn].ymom)/cv[idn].dens;
    }
    else
    {
      // supersonic flow (qn<0: inflow / qn>0: outflow)

      if (qn < 0.0)
      {
        cv[idn].dens = rhoinf;
        cv[idn].xmom = rhoinf*uinf;
        cv[idn].ymom = rhoinf*vinf;
        cv[idn].ener = pinf/gam1 + 0.5*rhoinf*qinf*qinf;
      }
      else
      {
        cv[idn].dens = cv[ibn].dens;
        cv[idn].xmom = cv[ibn].xmom;
        cv[idn].ymom = cv[ibn].ymom;
        cv[idn].ener = cv[ibn].ener;
      }
    }

    fluidProps.DependentVarsOne( idn );
    ibval++;
  } // ib
}