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

#ifndef DEFS_H_INCLUDED
#define DEFS_H_INCLUDED

#include <cuda_runtime.h>
#include <device_launch_parameters.h>

// floating point type (SGLPREC=single precision, otherwise double)
#define SGLPREC
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

void ParseCommandLine( int argc, char**argv, int *Ni, int *Nj,
                       int *maxIter, int *nThreadsX, int *nThreadsY );
cudaError_t Initialize( int Ni, int Nj, REAL *phiHost, REAL **phi, REAL **phiOld );
void Uninitialize( REAL **phi, REAL **phiOld );

// kernel function
__global__ void Jacobi( int Ni, int Nj, REAL *phi, REAL *phiOld );
__global__ void Jacobi_optim( int Ni, int Nj, int nThreadsX, int nThreadsY,
                              REAL *phi, REAL *phiOld );

#endif // DEFS_H_INCLUDED
