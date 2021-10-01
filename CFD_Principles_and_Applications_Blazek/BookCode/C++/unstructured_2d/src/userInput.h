/// @file userInput.h
///
/// Definition of the class related to user input.
///
//*****************************************************************************
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
//  Created February 15, 2014
//  Last modification: June 30, 2014
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

#ifndef USERINPUT_H_INCLUDED
#define USERINPUT_H_INCLUDED

#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include "defs.h"
#include "solver.h"
#include "output.h"

using std::cout;
using std::endl;
using std::left;
using std::scientific;
using std::setprecision;
using std::setfill;
using std::setw;
using std::ifstream;
using std::string;

/// @class UserInput
/// Encompasses functions used to read in and to process the user input file.
///
class UserInput
{
public:

  static void Read( const char *fileName, Solver &solver, Output &output );
  static void Print( const Solver &solver, const Output &output );

private:

  static void ReadVector( ifstream &stream, int nVals, bool vect[] );
  static void ReadVector( ifstream &stream, int nVals, REAL vect[] );

  /// Prints a value plus text in formatted form
  ///
  /// @param val  value to print
  /// @param txt  text to be formatted as comment
  ///
  static void PrintValue( int val, const char *txt )
  {
    cout << left << " " << setw(13) << setfill(' ')
         << val << "# " << txt << endl;
  }
  static void PrintValue( REAL val, const char *txt )
  {
    cout << left << scientific << setprecision(4) << setw(14) << setfill(' ')
      << val << "# " << txt << endl;
  }
  static void PrintValue( string val, const char *txt )
  {
    cout << left << setw(14) << setfill(' ')
         << val << "# " << txt << endl;
  }
};

#endif // USERINPUT_H_INCLUDED