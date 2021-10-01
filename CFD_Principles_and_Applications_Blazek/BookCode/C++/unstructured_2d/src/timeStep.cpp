/// @file timeStep.cpp
///
/// Computation of the time step.
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

#include "timeDiscr.h"

/// Computes the maximum stable time step.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param precond     low Mach-number preconditioning
/// @param order       order of the spatial discretization (1 or 2)
///
void TimeDiscr::TimeStep( const Geometry &geometry, const FluidProps &fluidProps,
                          const Precond &precond, int order )
{
  int  i;
  REAL sx, sy, ds, rrho, u, v, vc, cs, rhop, rhoT, hT, q2, theta, ra1g,
       a1, a4, a5, fmue, f1, f2, fac, dtv, cfac, lambdac, lambdav, tsmin;

  CONSVARS *cv    = fluidProps.cv;
  DEPVARS  *dv    = fluidProps.dv;
  NODE     *sproj = geometry.sproj;
  REAL     *vol   = geometry.vol;

  if (fluidProps.equsType == Equations::Euler)
  {
    // preconditioned Euler equations
    if (precond.switchedOn)
    {
      for (i=0; i<geometry.nndInt; i++)
      {
        sx       = sproj[i].x;
        sy       = sproj[i].y;
        ds       = sx + sy;
        u        = ABS(cv[i].xmom/cv[i].dens);
        v        = ABS(cv[i].ymom/cv[i].dens);
        rhop     =  cv[i].dens/dv[i].press;
        rhoT     = -cv[i].dens/dv[i].temp;
        hT       = dv[i].cpgas;
        q2       = u*u + v*v;
        theta    = precond.ComputeTheta( dv[i].gamma,dv[i].csoun,q2 );
        a1       = cv[i].dens*rhop*hT + rhoT;
        ra1g     = 1.0/(cv[i].dens*theta*hT+rhoT);
        a4       = a1*ra1g;
        a5       = cv[i].dens*hT*ra1g;
        vc       = (sx*u+sy*v)/ds;
        cs       = SQRT((vc*vc)*((a4-1.0)*(a4-1.0))+4.0*a5);
        tstep[i] = (2.0*vol[i])/((vc*(a4+1.0)+cs)*ds);
      }
    }
    // Euler equations
    else
    {
      for (i=0; i<geometry.nndInt; i++)
      {
        sx       = sproj[i].x;
        sy       = sproj[i].y;
        u        = ABS(cv[i].xmom/cv[i].dens);
        v        = ABS(cv[i].ymom/cv[i].dens);
        vc       = sx*u + sy*v;
        cs       = dv[i].csoun*(sx+sy);
        tstep[i] = vol[i]/(vc+cs);
      }
    }
  }
  else
  {
    VISCDEPVARS *dvLam = fluidProps.dvLam;
    if (order > 1)
      cfac = 1.0;
    else
      cfac = 2.0;

    // preconditioned Navier-Stokes equations (laminar)
    if (precond.switchedOn)
    {
      for (i=0; i<geometry.nndInt; i++)
      {
        sx       = sproj[i].x;
        sy       = sproj[i].y;
        ds       = sx + sy;
        rrho     = 1.0/cv[i].dens;
        u        = ABS(cv[i].xmom*rrho);
        v        = ABS(cv[i].ymom*rrho);
        rhop     =  cv[i].dens/dv[i].press;
        rhoT     = -cv[i].dens/dv[i].temp;
        hT       = dv[i].cpgas;
        q2       = u*u + v*v;
        theta    = precond.ComputeTheta( dv[i].gamma,dv[i].csoun,q2 );
        a1       = cv[i].dens*rhop*hT + rhoT;
        ra1g     = 1.0/(cv[i].dens*theta*hT+rhoT);
        a4       = a1*ra1g;
        a5       = cv[i].dens*hT*ra1g;
        vc       = (sx*u+sy*v)/ds;
        cs       = SQRT((vc*vc)*((a4-1.0)*(a4-1.0))+4.0*a5);
        fmue     = dvLam[i].mue/fluidProps.prlam;
        f1       = (4.0*rrho)/3.0;
        f2       = dv[i].gamma*rrho;
        fac      = MAX(f1,f2);
        dtv      = (fac*fmue)/vol[i];
        lambdac  = 0.5*((a4+1.0)*vc+cs)*ds;
        lambdav  = dtv*(sx*sx+sy*sy);
        tstep[i] = vol[i]/(lambdac+cfac*lambdav);
      }
    }
    // Navier-Stokes equations (laminar)
    else
    {
      for (i=0; i<geometry.nndInt; i++)
      {
        sx       = sproj[i].x;
        sy       = sproj[i].y;
        ds       = sx + sy;
        rrho     = 1.0/cv[i].dens;
        u        = ABS(cv[i].xmom*rrho);
        v        = ABS(cv[i].ymom*rrho);
        fmue     = dvLam[i].mue/fluidProps.prlam;
        f1       = (4.0*rrho)/3.0;
        f2       = dv[i].gamma*rrho;
        fac      = MAX(f1,f2);
        dtv      = (fac*fmue)/vol[i];
        vc       = sx*u + sy*v;
        cs       = dv[i].csoun*(sx+sy);
        lambdac  = vc + cs;
        lambdav  = dtv*(sx*sx+sy*sy);
        tstep[i] = vol[i]/(lambdac+cfac*lambdav);
      }
    }
  }

  // in case of global time-stepping - find min. time step in domain

  if (timeStepping == TimeStepping::Global)
  {
    tsmin = 1.0e+33;
    for (i=0; i<geometry.nndInt; i++)
    {
      tsmin = MIN(tsmin,tstep[i]);
    }
    for (i=0; i<geometry.nndInt; i++)
    {
      tstep[i] = tsmin;
    }
  }
}