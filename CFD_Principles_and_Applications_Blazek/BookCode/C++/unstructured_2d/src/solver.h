/// @file solver.h
///
/// Definition of the class related to the flow solver.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 5, 2014
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

#ifndef SOLVER_H_INCLUDED
#define SOLVER_H_INCLUDED

#include <string>
#include "defs.h"
#include "bndConds.h"
#include "fluidProps.h"
#include "geometry.h"
#include "output.h"
#include "precond.h"
#include "spaceDiscr.h"
#include "timeDiscr.h"

/// @class Solver
/// Encompasses variables and functions related to the flow solver. It takes care
/// of input, output and initialization of main solution and convergence data.
///
class Solver
{
public:
 
  BndConds   bndConds;    /**< boundary conditions */
  FluidProps fluidProps;  /**< fluid properties */
  Geometry   geometry;    /**< geometry and metrics */
  SpaceDiscr spaceDiscr;  /**< spatial discretization */
  TimeDiscr  timeDiscr;   /**< temporal discretization */
  Precond    precond;     /**< low Mach-number preconditioning */

  bool restUse;  /**< use of previous solution for restart (true, false) */
  int  maxIter,  /**< max. number of iterations */
       outStep,  /**< number of iterations between solution dumps */
       iter;     /**< actual iteration number */
  REAL convTol;  /**< convergence criterion (2-norm of density change for */
                 /**< which the iteration process is stopped) */
  
  std::string fnameRsti,  /**< file with restart solution - input */
              fnameRsto;  /**< file with restart solution - output */

  // functions

  Solver();
  void InitConstants();
  void InitSolution();
  void ReadSolution();
  void WriteSolution();
  void Convergence( Output &output );

  /// Returns true if solver has converged, otherwise false.
  ///
  bool Converged()
       { if (iter>=maxIter || drho<=convTol) return true;
         return false; }

private:

  REAL drho,    /**< change of the density residual (2-norm; convergence criterion) */
       drho1,   /**< initial change of the density residual (used for normalization) */
       cl,      /**< lift coefficient (pressure forces only; external flow) */
       cd,      /**< drag coefficient (pressure forces only; external flow) */
       cm,      /**< pitching moment coefficient wrp. to the reference point */
       mflow,   /**< average mass flow rate (internal flow) */
       mfratio; /**< ratio of mass flow at outlet to mass flow at inlet */

  // functions

  void Forces();
  void MassFlow();

  Solver( const Solver &solver );             // override default copy constructor
  Solver & operator = (const Solver &solver); // and assignment operator
};

#endif // SOLVER_H_INCLUDED
