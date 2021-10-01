/// @file streamIO.h
///
/// Functions for input/output operations on file streams.
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

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>

using std::ifstream;
using std::ofstream;
using std::runtime_error;
using std::string;

/// Reads a text line from file, strips any comment, leading and trailing spaces.
/// It is assumed that a comment starts with the hash (#) sign.
///
/// @param stream  file stream
/// @return        processed line as a string (might be empty!)
/// @exception     std::runtime_error  problem with the stream
///
inline string ReadLine( ifstream &stream )
{
  string str;
  string::size_type ic;

  if (stream.good())
  {
    getline( stream,str );

    // get rid of comment
    ic = str.find_first_of( '#' );
    if (ic != string::npos) str.erase( str.begin()+ic,str.end() );

    // get rid of leading spaces
    ic = str.find_first_not_of( ' ' );
    if ((int)ic > 0) str.erase( str.begin(),str.begin()+ic );

    // get rid of trailing spaces
    ic = str.find_last_not_of( ' ' );
    if (ic != string::npos) str.erase( str.begin()+ic+1,str.end() );

  }
  else throw runtime_error( "could not read line from file." );

  return str;
}

//*****************************************************************************

/// Reads the next value from a binary file.
///
/// @param stream  file stream
/// @param val     value in the file
/// @exception     std::runtime_error  problem with the stream
///
template <class T> void ReadBinary( ifstream &stream, T &val )
{
  if (stream.good())
    stream.read( (char*) &val,sizeof(T) );
  else
    throw runtime_error( "could not read data from file." );
}

//*****************************************************************************

/// Writes given value to a binary file.
///
/// @param stream  file stream
/// @param val     value to be written
///
template <class T> void WriteBinary( ofstream &stream, const T &val )
{
  stream.write( (char*) &val,sizeof(T) );
}