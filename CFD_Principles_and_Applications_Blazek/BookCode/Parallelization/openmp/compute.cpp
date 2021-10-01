//  Single iteration of the Jacobi scheme.
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

#include <omp.h>
#include "compute.h"

void Jacobi( int Ni, int Nj, REAL phi[], REAL phiOld[], REAL &resid )
{
  int  i, j, ij;
  REAL sum=0.0f;

  #pragma omp parallel default(none) shared(phi,phiOld,Ni,Nj,sum) private(i,j,ij) //num_threads(8)
  {
    // compute new solution

    #pragma omp for reduction(+ : sum) schedule(static) nowait
    for (j=1; j<(Nj-1); j++)
    {
      for (i=1; i<(Ni-1); i++)
      {
        ij      = i + j*Ni;
        phi[ij] = 0.25f*(phiOld[ij+1]+phiOld[ij-1]+phiOld[ij+Ni]+phiOld[ij-Ni]);
        sum    += (phi[ij]-phiOld[ij]) * (phi[ij]-phiOld[ij]);
      }
    }
  
  } // parallel region

  resid = SQRT(sum);
}