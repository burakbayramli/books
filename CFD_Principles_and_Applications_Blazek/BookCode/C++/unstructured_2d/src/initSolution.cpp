/// @file initSolution.cpp
///
/// Initialization of the flow solution.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 15, 2014
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
#include "solver.h"

/// Initializes conservative variables (initial guess). Ideal gas
/// is assumed (constant gas properties used everywhere).
///
void Solver::InitSolution()
{
  int  i, j, ib, ibn, ibegn, iendn;
  REAL gam1, rgas, xmin, xmax, dx, pinl, temp, rho,
       cs, mach, q, dp, dbeta, beta, p, u, v;

  gam1 = fluidProps.gamma - 1.0;
  rgas = gam1*fluidProps.cpgas/fluidProps.gamma;

  if (bndConds.flowType == FlowType::External)
  {
    // external flow

    for (i=0; i<geometry.nNodes; i++)
    {
      fluidProps.cv[i].dens = bndConds.rhoinf;
      fluidProps.cv[i].xmom = bndConds.rhoinf*bndConds.uinf;
      fluidProps.cv[i].ymom = bndConds.rhoinf*bndConds.vinf;
      fluidProps.cv[i].ener = bndConds.pinf/gam1 +
                              0.5*bndConds.rhoinf*bndConds.qinf*bndConds.qinf;
    }
  }
  else
  {
    // internal flow; inlet assumed at xmin, outlet at xmax; flow angle
    // and pressure are linearly interpolated between inlet and outlet

    xmin =  1.0e+32;
    xmax = -1.0e+32;
    for (i=0; i<geometry.nndInt; i++)
    {
      xmin = MIN(xmin,geometry.coords[i].x);
      xmax = MAX(xmax,geometry.coords[i].x);
    }
    dx = xmax - xmin;

    pinl  = bndConds.p12rat*bndConds.pout;
    if (pinl >= bndConds.ptinl) pinl = 0.99999*bndConds.ptinl;  // otherwise reversed flow at inlet
    dp    = bndConds.pout - pinl;
    dbeta = bndConds.betaout - bndConds.betainl;
    temp  = bndConds.ttinl * POW((pinl/bndConds.ptinl),(gam1/fluidProps.gamma));
    rho   = pinl/(rgas*temp);
    cs    = SQRT(fluidProps.gamma*pinl/rho);
    mach  = SQRT(2.0*((bndConds.ttinl/temp)-1.0)/gam1);
    q     = mach*cs;

    for (i=0; i<geometry.nNodes; i++)
    {
      beta = bndConds.betainl + dbeta*(geometry.coords[i].x-xmin)/dx;
      p    = pinl + dp*(geometry.coords[i].x-xmin)/dx;
      rho  = p/(rgas*temp);
      u    = q*COS(beta);
      v    = q*SIN(beta);
      fluidProps.cv[i].dens = rho;
      fluidProps.cv[i].xmom = rho*u;
      fluidProps.cv[i].ymom = rho*v;
      fluidProps.cv[i].ener = p/gam1 + 0.5*rho*q*q;
    }
  }

  // equalize flow variables at periodic nodes

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
        fluidProps.cv[i].dens = 0.5*(fluidProps.cv[i].dens+fluidProps.cv[j].dens);
        fluidProps.cv[i].xmom = 0.5*(fluidProps.cv[i].xmom+fluidProps.cv[j].xmom);
        fluidProps.cv[i].ymom = 0.5*(fluidProps.cv[i].ymom+fluidProps.cv[j].ymom);
        fluidProps.cv[i].ener = 0.5*(fluidProps.cv[i].ener+fluidProps.cv[j].ener);
        fluidProps.cv[j].dens = fluidProps.cv[i].dens;
        fluidProps.cv[j].xmom = fluidProps.cv[i].xmom;
        fluidProps.cv[j].ymom = fluidProps.cv[i].ymom;
        fluidProps.cv[j].ener = fluidProps.cv[i].ener;
      }
    }
    ibegn = iendn + 1;
  }
}