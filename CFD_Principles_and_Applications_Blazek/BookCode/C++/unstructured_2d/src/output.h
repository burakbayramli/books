/// @file output.h
///
/// Definition of the class providing output of plot files
/// and of the convergence history.
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

#ifndef OUTPUT_H_INCLUDED
#define OUTPUT_H_INCLUDED

#include <fstream>
#include <iostream>
#include <string>
#include "defs.h"
#include "bndConds.h"
#include "spaceDiscr.h"

/// @class Output
/// Encompasses data and functions required to output plot data
/// and the convergence history. The plot data itself is provided
/// by the class Solver.
///
class Output
{
public:

  static const int MXQUANT =13,  /**< total number of plot variables */
                   MXQFIELD=11;  /**< no. of plot variables in the field */
                                 /**< (cf and Cp only at the boundaries) */

  // variables

  std::string title,      /**< title of the simulation case */
              fnameFlow,  /**< flow field (+ 5 digit iteration number + .v2d) */
              fnameSurf,  /**< quantities along wall surface(s) (+ 5 digit iteration number + .v2d) */
              fnameConv;  /**< convergence history (+ .v2d) */
  bool varOn[MXQUANT];    /**< on/off switches of the plot variables */

  // functions

  Output();
  void Flowfield( const Geometry &geometry, const FluidProps &fluidProps,
                  const BndConds &bndConds, int iter ) const;
  void Surfaces( const Geometry &geometry, const FluidProps &fluidProps,
                 const BndConds &bndConds, const SpaceDiscr &spaceDiscr, int iter ) const;
  void OpenConvergence();
  void WriteConvergence( int iter, REAL drho, REAL drmax, int idrmax,
                         REAL cl, REAL cd, REAL cm, REAL mflow, REAL mfratio );
  void CloseConvergence();

  /// Returns name of a plot variable as a string.
  ///
  /// @param index  index of the variable
  ///
  std::string GetVarName( int index ) const { return varName[MAX(0,MIN(index,MXQUANT-1))]; }

  /// Stores the type of flow for later use when saving flow results.
  ///
  /// @param flowType  type of flow (external or internal)
  ///
  void StoreFlowType( FlowType flowType )
       { external = (flowType==FlowType::External); }

private:

  bool external;                 /**< stores whether flow is external or not */
  std::string varName[MXQUANT];  /**< names of plot variables */
  std::ofstream convStream;      /**< stream for convergence history */

  // functions

  Output( const Output &output );             // override default copy constructor
  Output & operator = (const Output &output); // and assignment operator
};

#endif // OUTPUT_H_INCLUDED