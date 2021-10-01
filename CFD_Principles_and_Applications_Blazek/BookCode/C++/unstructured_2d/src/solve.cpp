/// @file solve.cpp
///
/// Single iteration of the governing equations.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: September 20, 2014
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

/// Integrates the four basic equations (continuity, momentum and energy) by
/// the explicit, multi-stage (Runge-Kutta) time-stepping scheme.
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param bndConds    boundary conditions
/// @param spaceDiscr  spatial discretization
/// @param precond     low Mach-number preconditioning
///
void TimeDiscr::Solve( const Geometry &geometry, FluidProps &fluidProps, BndConds &bndConds,
                       SpaceDiscr &spaceDiscr, const Precond &precond )
{
  int  i, irk;
  REAL fac, adtv, H, q2, rhop, rhoT, hT, theta, u, v;
  REAL wvec[5], wpvec[5], pmat[5][5], gmat1[5][5], mat[5][5], r[5];

  CONSVARS *cv  = fluidProps.cv;
  DEPVARS  *dv  = fluidProps.dv;
  CONSVARS *rhs = spaceDiscr.rhs;

  // store previous solution

  for (i=0; i<geometry.nNodes; i++) cvOld[i] = cv[i];

  // compute the time step

  TimeStep( geometry,fluidProps,precond,spaceDiscr.order );

  // loop over the Runge-Kutta stages *****************************************

  for (irk=0; irk<nrk; irk++)
  {
    // initialize dissipation

    if (dissipOn[irk])
      spaceDiscr.DissipInit( irk,geometry.nNodes,betrk[irk] );

    // viscous flux (Navier-Stokes equations)

    if (dissipOn[irk] && fluidProps.equsType==Equations::NavierStokes)
    {
      spaceDiscr.GradientsVisc( geometry,fluidProps );
      spaceDiscr.FluxViscous( geometry,fluidProps,betrk[irk] );
    }

    // Roe's flux-difference splitting scheme (upwind)

    // limiter and upwind dissipation
    if (dissipOn[irk])
    {
      if (spaceDiscr.order < 2)
      {
        if (precond.switchedOn)
          spaceDiscr.DissipRoe1Prec( geometry,fluidProps,precond,betrk[irk] );
        else
          spaceDiscr.DissipRoe1( geometry,fluidProps,betrk[irk] );
      }
      else
      {
        if (fluidProps.equsType==Equations::Euler)
          spaceDiscr.Gradients( geometry,fluidProps );
        spaceDiscr.LimiterInit( geometry,fluidProps );
        spaceDiscr.Limiter( geometry,fluidProps );
        if (precond.switchedOn)
          spaceDiscr.DissipRoe2Prec( geometry,fluidProps,precond,betrk[irk] );
        else
          spaceDiscr.DissipRoe2( geometry,fluidProps,betrk[irk] );
      }
    }

    // convective flux; add upwind dissipation => residual
    if (spaceDiscr.order < 2)
      spaceDiscr.FluxRoe1( geometry,fluidProps );
    else
      spaceDiscr.FluxRoe2( geometry,fluidProps );

    // preconditioning

    if (precond.switchedOn)
    {
      wvec[3]  = 0.0;  // no 3rd dimension here
      wpvec[3] = 0.0;

      for (i=0; i<geometry.nndInt; i++)
      {
        rhop  =  cv[i].dens/dv[i].press;
        rhoT  = -cv[i].dens/dv[i].temp;
        hT    = dv[i].cpgas;
        u     = cv[i].xmom/cv[i].dens;
        v     = cv[i].ymom/cv[i].dens;
        q2    = u*u + v*v;
        H     = (cv[i].ener+dv[i].press)/cv[i].dens;
        theta = precond.ComputeTheta( dv[i].gamma,dv[i].csoun,q2 );

        wvec[0]  = cv[i].dens;
        wvec[1]  = cv[i].xmom;
        wvec[2]  = cv[i].ymom;
        wvec[4]  = cv[i].ener;
        wpvec[0] = dv[i].press;
        wpvec[1] = u;
        wpvec[2] = v;
        wpvec[4] = dv[i].temp;

        precond.Cons2Prim( wvec,wpvec,H,q2,theta,rhoT,0.0,hT,gmat1 );
        precond.Prim2Cons( wvec,wpvec,H,rhop,rhoT,0.0,hT,pmat );
        precond.MatrixTimesInverse( wpvec,q2,pmat,gmat1,mat );
        r[0]        = rhs[i].dens;
        r[1]        = rhs[i].xmom;
        r[2]        = rhs[i].ymom;
        r[4]        = rhs[i].ener;
        rhs[i].dens = mat[0][0]*r[0] + mat[0][1]*r[1] +
                      mat[0][2]*r[2] + mat[0][4]*r[4];
        rhs[i].xmom = mat[1][0]*r[0] + mat[1][1]*r[1] +
                      mat[1][2]*r[2] + mat[1][4]*r[4];
        rhs[i].ymom = mat[2][0]*r[0] + mat[2][1]*r[1] +
                      mat[2][2]*r[2] + mat[2][4]*r[4];
        rhs[i].ener = mat[4][0]*r[0] + mat[4][1]*r[1] +
                      mat[4][2]*r[2] + mat[4][4]*r[4];
      }
    }

    // correct residuals at symmetry/no-slip boundaries

    bndConds.ZeroResiduals( geometry,fluidProps.equsType,rhs );

    // combine residuals at periodic boundaries

    bndConds.Periodic( geometry,rhs );

    // residual * time step / volume

    fac = ark[irk]*cfl;
    for (i=0; i<geometry.nndInt; i++)
    {
      adtv         = fac*tstep[i]/geometry.vol[i];
      rhs[i].dens *= adtv;
      rhs[i].xmom *= adtv;
      rhs[i].ymom *= adtv;
      rhs[i].ener *= adtv;
    }

    // implicit residual smoothing

    if (epsIrs > 0.0)
    {
      Irsmoo( geometry,spaceDiscr );
      bndConds.ZeroResiduals( geometry,fluidProps.equsType,rhs );
    }

    // update - new solution, new dependent variables

    for (i=0; i<geometry.nndInt; i++)
    {
      cv[i].dens = cvOld[i].dens - rhs[i].dens;
      cv[i].xmom = cvOld[i].xmom - rhs[i].xmom;
      cv[i].ymom = cvOld[i].ymom - rhs[i].ymom;
      cv[i].ener = cvOld[i].ener - rhs[i].ener;
    }
    fluidProps.DependentVarsAll( geometry.nndInt );

    // boundary conditions

    bndConds.BoundaryConditions( geometry,fluidProps,precond );

  } // irk
}