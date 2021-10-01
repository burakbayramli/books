/// @file spaceDiscr.h
///
/// Definition of the class related to spatial discretization.
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

#ifndef SPACEDISCR_H_INCLUDED
#define SPACEDISCR_H_INCLUDED

#include "defs.h"
#include "bndConds.h"
#include "fluidProps.h"
#include "geometry.h"
#include "precond.h"

/// @class SpaceDiscr
/// Encompasses data and functions related to spatial discretization
/// of the governing equations (Roe's upwind scheme).
///
class SpaceDiscr
{
public:
 
  int  order;     /**< order of the scheme (1 or 2) */
  REAL limfac,    /**< limiter coefficient */
       epsEntr;   /**< entropy correction coefficient */

  CONSVARS *rhs;  /**< residuals (right-hand side) */

  // functions

  SpaceDiscr();
  ~SpaceDiscr();
  void AllocateMemory( Equations equsType, int nNodes );
  void DissipInit( int irk, int nNodes, REAL beta );
  void DissipRoe1( const Geometry &geometry, const FluidProps &fluidProps, REAL beta );
  void DissipRoe1Prec( const Geometry &geometry, const FluidProps &fluidProps,
                       const Precond &precond, REAL beta );
  void DissipRoe2( const Geometry &geometry, const FluidProps &fluidProps, REAL beta );
  void DissipRoe2Prec( const Geometry &geometry, const FluidProps &fluidProps,
                       const Precond &precond, REAL beta );
  void FluxRoe1( const Geometry &geometry, const FluidProps &fluidProps );
  void FluxRoe2( const Geometry &geometry, const FluidProps &fluidProps );
  void FluxWalls( const Geometry &geometry, const FluidProps &fluidProps );
  void FluxViscous( const Geometry &geometry, const FluidProps &fluidProps, REAL beta );
  void Gradients( const Geometry &geometry, const FluidProps &fluidProps );
  void GradientsVisc( const Geometry &geometry, const FluidProps &fluidProps );
  void LimiterRefvals( const Geometry &geometry, const FluidProps &fluidProps,
                       const BndConds &bndConds );
  void LimiterInit( const Geometry &geometry, const FluidProps &fluidProps );
  void Limiter( const Geometry &geometry, const FluidProps &fluidProps );

  /// Returns gradients of u-velocity with respect to x and y.
  ///
  /// @param i   node index
  /// @param du  du/dx and du/dy
  ///
  void GetGradU( int i, NODE &du ) const
       { du.x = gradx[i].uvel; du.y = grady[i].uvel; }

  /// Returns gradients of v-velocity with respect to x and y.
  ///
  /// @param i   node index
  /// @param dv  dv/dx and dv/dy
  ///
  void GetGradV( int i, NODE &dv ) const
       { dv.x = gradx[i].vvel; dv.y = grady[i].vvel; }

private:

  REAL     volRef;  /**< reference volume. Parameter is required for the computation of limiter */
                    /**< functions (higher-order scheme). */
  PRIMVARS limRef;  /**< reference values of density, u, v and pressure. Parameter is required */
                    /**< for the computation of limiter functions (higher-order scheme). */

  CONSVARS *diss;   /**< artificial dissipation */
  PRIMVARS *lim;    /**< values of the limiter function (density, u, v, pressure) */
  PRIMVARS *gradx,  /**< gradients of density, velocity components and of pressure */
                    /**< with respect to the x-coordinate (node-based) */
           *grady;  /**< gradients of density, velocity components and of pressure */
                    /**< with respect to the y-coordinate (node-based) */
  REAL     *gradTx, /**< gradient of temperature with respect to the x-coordinate */
           *gradTy; /**< gradient of temperature with respect to the y-coordinate */

  PRIMVARS *umin,   /**< minimum of U_i and of min_j U_j (U = rho, u, v, p) */
           *umax;   /**< maximum of U_i and of max_j U_j (U = rho, u, v, p) */

  // functions

  SpaceDiscr( const SpaceDiscr &spaceDiscr );             // override default copy constructor
  SpaceDiscr & operator = (const SpaceDiscr &spaceDiscr); // and assignment operator

  /// Evaluates entropy correction function
  ///
  /// @param z  value to be corrected
  /// @param d  threshold value of the correction
  /// @return   corrected value of z
  ///
  REAL EntropyCorr( REAL z, REAL d )
  {
    if (z > d)
      return z;
    else
      return 0.5*(z*z+d*d)/d;
  }

  ///  Evaluates the limiter function (Venkatakrishnan's limiter)
  ///
  /// @param d2     change of value to be limited (gradient * distance)
  /// @param d1min  min(U_i, U_j) - U_i
  /// @param d1max  max(U_i, U_j) - U_i
  /// @param eps2   threshold value
  /// @return       limiter value
  ///
  REAL Venkat( REAL d2, REAL d1min, REAL d1max, REAL eps2 )
  {
    if (d2 > EPSGLO)
    {
      REAL num = (d1max*d1max+eps2)*d2 + 2.0*d2*d2*d1max;
      REAL den = d2*(d1max*d1max+2.0*d2*d2+d1max*d2+eps2);
      return (num/den);
    }
    else if (d2 < -EPSGLO)
    {
      REAL num = (d1min*d1min+eps2)*d2 + 2.0*d2*d2*d1min;
      REAL den = d2*(d1min*d1min+2.0*d2*d2+d1min*d2+eps2);
      return (num/den);
    }
    return 1.0;
  }
};

#endif // SPACEDISCR_H_INCLUDED