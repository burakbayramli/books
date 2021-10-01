//  Definitions and constants required for the example.
//
//  (c) J. Blazek, CFD Consulting & Analysis, www.cfd-ca.de
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

#ifndef COMPUTE_H_INCLUDED
#define COMPUTE_H_INCLUDED

#include <cstdlib>
#include <cmath>

// floating point type (SGLPREC=single precision, otherwise double)
#ifdef SGLPREC
  typedef float  REAL;
  #define SQRT   sqrtf
#else
  typedef double REAL;
  #define SQRT   sqrt
#endif

#ifndef STRNCASECMP
#ifdef _WIN32
#define STRNCASECMP _strnicmp
#else
#define STRNCASECMP strncasecmp
#endif
#endif

void Jacobi( int Ni, int Nj, REAL phi[], REAL phiOld[], REAL &resid );
void ParseCommandLine( int argc, char **argv, int &Ni, int &Nj, int &maxIter );

#endif // COMPUTE_H_INCLUDED