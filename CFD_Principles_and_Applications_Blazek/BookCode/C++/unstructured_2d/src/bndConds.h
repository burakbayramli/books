/// @file bndConds.h
///
/// Definition of the class related to boundary conditions.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 18, 2014
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

#ifndef BNDCONDS_H_INCLUDED
#define BNDCONDS_H_INCLUDED

#include "defs.h"
#include "fluidProps.h"
#include "geometry.h"
#include "precond.h"

/// @class BndConds
/// Encompasses data and functions related to the application of boundary
/// conditions. Topology of the boundaries itself is contained in the
/// class Geometry.
///
class BndConds
{
public:
  
  FlowType flowType;  /**< type of flow (external or internal) */

  // boundary conditions - external flow

  bool vortCorr; /**< far-field vortex correction (true or false) */
  REAL machinf,  /**< Mach-number at infinity (far-field) */
       alpha,    /**< angle of attack */
       pinf,     /**< static pressure at infinity */
       tinf,     /**< static temperature at infinity */
       rhoinf,   /**< density at infinity */
       uinf,     /**< u-component of velocity vector at infinity */
       vinf,     /**< v-component of velocity vector at infinity */
       qinf,     /**< total velocity (= SQRT(uinf**2+vinf**2)) */
       cl;       /**< lift coefficient (pressure forces only; used for vortex correction) */

  // boundary conditions - internal flow

  REAL ptinl,    /**< total pressure at inlet */
       ttinl,    /**< total temperature at inlet */
       betainl,  /**< low angle at inlet (with x-axis, positive in the clock-wise direction) */
       betaout,  /**< approximate outlet angle (utilized for the initial guess only) */
       p12rat,   /**< ratio of inlet to outlet static pressure (initial guess only) */
       pout;     /**< static pressure at outlet */

  // functions

  BndConds();
  ~BndConds();
  void AllocateMemory( const Geometry &geometry );
  void BoundaryConditions( const Geometry &geometry, FluidProps &fluidProps,
                           const Precond &precond );
  static void Periodic( const Geometry &geometry, CONSVARS var[] );
  static void Periodic( const Geometry &geometry, PRIMVARS var[] );
  static void Periodic( const Geometry &geometry, REAL var1[], REAL var2[] );
  static void Periodic( const Geometry &geometry, int var[] );
  static void ZeroResiduals( const Geometry &geometry, Equations equsType, CONSVARS rhs[] );

private:

  PRIMVARS *bndVals;  /**< boundary values of density, u, v, and p (used for far-field BC) */

  // functions

  BndConds( const BndConds &bndConds );             // override default copy constructor
  BndConds & operator = (const BndConds &bndConds); // and assignment operator
  void BcondInflow( const Geometry &geometry, FluidProps &fluidProps, int ibegn, int iendn ) const;
  void BcondOutflow( const Geometry &geometry, FluidProps &fluidProps, int ibegn, int iendn ) const;
  void BcondFarfield( const Geometry &geometry, FluidProps &fluidProps,
                      const Precond &precond, int ibegn, int iendn );
  void BcondWallVisc( const Geometry &geometry, FluidProps &fluidProps, int ibegn, int iendn ) const;
};

#endif // BNDCONDS_H_INCLUDED