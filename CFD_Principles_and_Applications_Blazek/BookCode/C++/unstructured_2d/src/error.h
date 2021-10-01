/// @file error.h
///
/// Definition of the class related to error messages.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 11, 2014
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

#ifndef ERROR_H_INCLUDED
#define ERROR_H_INCLUDED

#include <cstdlib>
#include <iostream>

/// @class Error
/// Encompasses functions related to the output of error messages.
///
class Error
{
public:
  /// Writes given error message to the standard output and exits the program.
  ///
  /// @param message  text of the error message
  ///
  static void Message( const char *message )
  {
    using namespace std;
    cout << "Error: " << message << endl << endl;
    exit(EXIT_FAILURE);
  }
};

#endif // ERROR_H_INCLUDED