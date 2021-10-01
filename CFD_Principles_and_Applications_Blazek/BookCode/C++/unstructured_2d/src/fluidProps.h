/// @file fluidProps.h
///
/// Definition of the class related to fluid properties.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 3, 2014
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

#ifndef FLUIDPROPS_H_INCLUDED
#define FLUIDPROPS_H_INCLUDED

#include "defs.h"

/// @class FluidProps
/// Encompasses variables and functions related to fluid properties
/// and to the dependent variables. Note that all values are given
/// in SI units.
///
class FluidProps
{
public:

  // variables

  Equations equsType;  /**< equations solved (Euler or Navier-Stokes) */

  REAL gamma,    /**< ratio of specific heat coefficients */
       cpgas,    /**< specific heat coefficient at constant pressure */
       prlam,    /**< laminar Prandtl number */
       renum,    /**< Reynolds number */
       refVel,   /**< reference velocity (internal flow only; for external flow computed */
                 /**< from the far-field boundary) */
       refRho,   /**< reference density (internal flow only; for external flow computed */
                 /**< from the far-field boundary) */
       refVisc;  /**< reference dynamic viscosity coefficient (computed from renum, refVel, */
                 /**< cref and refRho) */

  CONSVARS    *cv;     /**< conserved variables (rho, rho*u, rho*v, rho*E) */
  DEPVARS     *dv;     /**< dependent variables (p, T, c, gamma, c<sub>p</sub> */
  VISCDEPVARS *dvLam;  /**< dependent variables for laminar flow (µ<sub>L</sub>, k<sub>L</sub>) */

  // functions

  FluidProps();
  ~FluidProps();
  void AllocateMemory( int nNodes );
  void DependentVarsAll( int nNodes );
  void DependentVarsOne( int i );

private:

  FluidProps( const FluidProps &fluidProps );             // override default copy constructor
  FluidProps & operator = (const FluidProps &fluidProps); // and assignment operator
};

#endif // FLUIDPROPS_H_INCLUDED
