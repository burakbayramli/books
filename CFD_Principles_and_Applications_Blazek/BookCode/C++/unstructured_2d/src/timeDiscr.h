/// @file timeDiscr.h
///
/// Definition of the class related to temporal discretization.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 8, 2014
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

#ifndef TIMEDISCR_H_INCLUDED
#define TIMEDISCR_H_INCLUDED

#include "defs.h"
#include "bndConds.h"
#include "fluidProps.h"
#include "geometry.h"
#include "precond.h"
#include "spaceDiscr.h"

/// @class TimeDiscr
/// Encompasses data and functions related to temporal discretization
/// of the governing equations (time stepping).
///
class TimeDiscr
{
public:

  // variables

  TimeStepping timeStepping;  /**< switch between local and global time-stepping */

  int  nIterIrs,     /**< number of Jacobi iterations (implicit residual smoothing) */
       nrk;          /**< number of stages (Runge-Kutta scheme); max. = 5 */
  bool dissipOn[5];  /**< dissipation evaluation per stage (0=no, 1=yes) */
  REAL cfl,          /**< CFL-number */
       epsIrs,       /**< coefficient of implicit residual smoothing */
       ark[5],       /**< stage coefficients */
       betrk[5];     /**< dissipation-blending coefficients */

  // functions

  TimeDiscr();
  ~TimeDiscr();
  void AllocateMemory( int nNodes );
  void DensityChange( const Geometry &geometry, const FluidProps &fluidProps,
                      REAL &drho, REAL &drmax, int &idrmax ) const;
  void Irsmoo( const Geometry &geometry, SpaceDiscr &spaceDiscr );
  void Solve( const Geometry &geometry, FluidProps &fluidProps, BndConds &bndConds,
              SpaceDiscr &spaceDiscr, const Precond &precond );
  void TimeStep( const Geometry &geometry, const FluidProps &fluidProps,
                 const Precond &precond, int order );

private:

  CONSVARS *cvOld;    /**< conservative variables from the previous time step */
  REAL     *tstep;    /**< allowable time steps (without the CFL-number) */

  // temporary storage required for implicit residual smoothing (to avoid repeated alloc./dealloc.)
  CONSVARS *rhsOld,   /**< initial non-smoothed residual */
           *rhsIter;  /**< nodal contributions to the residual during the iteration */
  int      *nContr;   /**< number of contributions to a node */

  // functions

  TimeDiscr( const TimeDiscr &timeDiscr );             // override default copy constructor
  TimeDiscr & operator = (const TimeDiscr &timeDiscr); // and assignment operator
};

#endif // TIMEDISCR_H_INCLUDED