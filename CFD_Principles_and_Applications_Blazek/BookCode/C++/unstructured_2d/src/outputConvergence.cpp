/// @file outputConvergence.cpp
///
/// Output of the convergence history in Vis2D format.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: July 4, 2014
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

#include <stdexcept>
#include "output.h"

using namespace std;

/// Opens file for the convergence history and writes the header.
///
/// @exception std::runtime_error  file cannot be opened
///
void Output::OpenConvergence()
{
  string fname = fnameConv + ".v2d";

  convStream = ofstream( fname,ios::out | ios::trunc,_SH_DENYWR );
  if (convStream.fail()) throw runtime_error( "could not open convergence file for writing." );

  convStream << title << endl;
  convStream << "1" << endl;
  convStream << "Convergence History" << endl;

  if (external)
  {
    convStream << "1 7" << endl
               << "step" << endl
               << "resid" << endl
               << "resmax" << endl
               << "i-res" << endl
               << "cl" << endl
               << "cd" << endl
               << "cm" << endl
               << "-1 0" << endl
               << "0 0 0" << endl
               << "Unstructured" << endl;
  }
  else
  {
    convStream << "1 6" << endl
               << "step" << endl
               << "resid" << endl
               << "resmax" << endl
               << "i-res" << endl
               << "mass-flow" << endl
               << "mass-ratio" << endl
               << "-1 0" << endl
               << "0 0 0" << endl
               << "Unstructured" << endl;
  }

  convStream << scientific;
}

//*****************************************************************************

/// Writes current convergence results to a file.
///
/// @param iter    iteration number
/// @param drho    change of the density residual (2-norm)
/// @param drmax   maximum change of the density residual
/// @param idrmax  index of the node with the largest residual
/// @param cl      lift coefficient
/// @param cd      drag coefficient
/// @param cm      pitching moment coefficient
/// @param mflow   average mass flow rate
/// @param mfratio ratio of mass flow at outlet to mass flow at inlet
///
void Output::WriteConvergence( int iter, REAL drho, REAL drmax, int idrmax,
                               REAL cl, REAL cd, REAL cm, REAL mflow, REAL mfratio )
{
  if (external)
  {
    convStream << iter << " "
               << LOG10(drho) << " "
               << drmax << " "
               << idrmax << " "
               << cl << " "
               << cd << " "
               << cm << endl;
  }
  else
  {
    convStream << iter << " "
               << LOG10(drho) << " "
               << drmax << " "
               << idrmax << " "
               << mflow << " "
               << mfratio << endl;
  }
}

//*****************************************************************************

/// Closes file with the convergence history.
///
void Output::CloseConvergence()
{
  convStream.close();
}