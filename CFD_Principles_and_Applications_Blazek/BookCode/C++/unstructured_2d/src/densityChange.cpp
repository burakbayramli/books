/// @file densityChange.cpp
///
/// Computation of the 2-norm of the density change.
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

#include "timeDiscr.h"

/// Computes of the 2-norm of the density change (density residual).
///
/// @param geometry    geometrical data
/// @param fluidProps  fluid properties
/// @param drho        change of the density residual (2-norm)
/// @param drmax       maximum change of the density residual
/// @param idrmax      index of the node with the largest residual
///
void TimeDiscr::DensityChange( const Geometry &geometry, const FluidProps &fluidProps,
                               REAL &drho, REAL &drmax, int &idrmax ) const
{
  int  i;
  REAL dr;

  drho   = 0.0;
  drmax  = 0.0;
  idrmax = 0;
  for (i=0; i<geometry.nndInt; i++)
  {
    dr    = fluidProps.cv[i].dens - cvOld[i].dens;
    drho += dr*dr;
    if (ABS(dr) >= drmax)
    {
      drmax  = ABS(dr);
      idrmax = i;
    }
  }

  drho = SQRT(drho);
}