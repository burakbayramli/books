/// @file output.cpp
///
/// Initialization of data related to plot output.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 14, 2014
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

#include "output.h"

/// Initializes data related to plot output.
///
Output::Output()
{
  title.clear();
  fnameFlow.clear();
  fnameSurf.clear();
  fnameConv.clear();

  for (int i=0; i<MXQUANT; i++)
  {
    varOn[i] = false;
  }

  // private variables

  varName[ 0] = "density [kg/m^3]"; /* density */
  varName[ 1] = "u [m/s]";          /* u-velocity */
  varName[ 2] = "v [m/s]";          /* v-velocity */
  varName[ 3] = "p [Pa]";           /* static pressure */
  varName[ 4] = "p-tot [Pa]";       /* total pressure */
  varName[ 5] = "T [K]";            /* static temperature */
  varName[ 6] = "T-tot [K]";        /* total temperature */
  varName[ 7] = "M";                /* local Mach-number */
  varName[ 8] = "M-isen";           /* isentropic Mach-number */
  varName[ 9] = "pt-loss";          /* total pressure loss */
  varName[10] = "visc-lam [kg/ms]"; /* laminar viscosity */
  varName[11] = "cf";               /* friction coefficient (boundaries only) */
  varName[12] = "-Cp";              /* pressure coefficient (boundaries only) */

  external = true;
}
