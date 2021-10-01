/// @file dependentVars.cpp
///
/// Computation of dependent variables under the assumption of ideal gas
/// with constant properties.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 1, 2014
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
#include "fluidProps.h"

/// Computes values of dependent variables (pressure, temperature, speed
/// of sound, specific heat ratio, specific heat coeff. at const. pressure)
/// from conservative variables at all grid points. Additionally, laminar
/// viscosity and heat conductivity coefficients are computed in the case
/// of viscous flow.
///
/// @param nNodes  number of nodes
///
void FluidProps::DependentVarsAll( int nNodes )
{
  REAL gam1, rgas, g1cp, rhoq, s1, s2, s12, rat, cppr;

  gam1 = gamma - 1.0;
  rgas = gam1*cpgas/gamma;
  g1cp = gam1*cpgas;

  if (equsType == Equations::Euler)  // Euler equations
  {   
    for (int i=0; i<nNodes; i++)
    {
      rhoq        = cv[i].xmom*cv[i].xmom + cv[i].ymom*cv[i].ymom;
      dv[i].press = gam1*(cv[i].ener-0.5*rhoq/cv[i].dens);
      dv[i].temp  = dv[i].press/(rgas*cv[i].dens);
      dv[i].csoun = SQRT(g1cp*dv[i].temp);
      dv[i].gamma = gamma;
      dv[i].cpgas = cpgas;
    }
  }
  else  // Navier-Stokes equations
  {
    s1   = 110.0;
    s2   = 288.16;
    s12  = 1.0 + s1/s2;
    cppr = cpgas/prlam;

    for (int i=0; i<nNodes; i++)
    {
      rhoq          = cv[i].xmom*cv[i].xmom + cv[i].ymom*cv[i].ymom;
      dv[i].press   = gam1*(cv[i].ener-0.5*rhoq/cv[i].dens);
      dv[i].temp    = dv[i].press/(rgas*cv[i].dens);
      dv[i].csoun   = SQRT(g1cp*dv[i].temp);
      dv[i].gamma   = gamma;
      dv[i].cpgas   = cpgas;
      rat           = SQRT(dv[i].temp/s2)*s12/(1.0+s1/dv[i].temp);
      dvLam[i].mue  = refVisc*rat;
      dvLam[i].cond = dvLam[i].mue*cppr;
    }
  }
}

//*****************************************************************************

/// Computes values of dependent variables (pressure, temperature, speed
/// of sound, specific heat ratio, specific heat coeff. at const. pressure)
/// from conservative variables at the node i. Additionally, laminar
/// viscosity and heat conductivity coefficients are computed in the case
/// of viscous flow.
///
/// @param i  node index
///
void FluidProps::DependentVarsOne( int i )
{
  REAL gam1, rgas, g1cp, rhoq, s1, s2, s12, rat;

  gam1 = gamma - 1.0;
  rgas = gam1*cpgas/gamma;
  g1cp = gam1*cpgas;

  if (equsType == Equations::Euler)  // Euler equations
  {
    rhoq        = cv[i].xmom*cv[i].xmom + cv[i].ymom*cv[i].ymom;
    dv[i].press = gam1*(cv[i].ener-0.5*rhoq/cv[i].dens);
    dv[i].temp  = dv[i].press/(rgas*cv[i].dens);
    dv[i].csoun = SQRT(g1cp*dv[i].temp);
    dv[i].gamma = gamma;
    dv[i].cpgas = cpgas;
  }
  else  // Navier-Stokes equations
  {
    s1  = 110.0;
    s2  = 288.16;
    s12 = 1.0 + s1/s2;

    rhoq          = cv[i].xmom*cv[i].xmom + cv[i].ymom*cv[i].ymom;
    dv[i].press   = gam1*(cv[i].ener-0.5*rhoq/cv[i].dens);
    dv[i].temp    = dv[i].press/(rgas*cv[i].dens);
    dv[i].csoun   = SQRT(g1cp*dv[i].temp);
    dv[i].gamma   = gamma;
    dv[i].cpgas   = cpgas;
    rat           = SQRT(dv[i].temp/s2)*s12/(1.0+s1/dv[i].temp);
    dvLam[i].mue  = refVisc*rat;
    dvLam[i].cond = dvLam[i].mue*(cpgas/prlam);
  }
}