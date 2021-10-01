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

#include "compute.h"

void Compute::Jacobi( REAL &resid )
{
  int i, j, ij;

  resid = 0.0f;

  for (j=jbeg; j<=jend; j++)
  {
    for (i=1; i<(Ni-1); i++)
    {
      ij      = i + j*Ni;
      phi[ij] = 0.25f*(phiOld[ij+1]+phiOld[ij-1]+phiOld[ij+Ni]+phiOld[ij-Ni]);
      resid  += (phi[ij]-phiOld[ij]) * (phi[ij]-phiOld[ij]);
    }
  }

  REAL *tmp = phiOld;  // swap old and new solution
  phiOld    = phi;
  phi       = tmp;

  // send residual (all except task 0);
  // receive and sum up residuals by task 0
  // send final residual by task 0 and receive it by all the other tasks

  if (task > 0) asend( sendRes,resid );

  if (task == 0)
  {
    REAL res=0;
    for (i=1; i<nProcs; i++)
    {
      res    = receive( recvRes );
      resid += res;
    }
    resid = SQRT(resid);
    for (i=1; i<nProcs; i++)
    {
      asend( sendRes,resid );
    }
  }
  else
  {
    resid = receive( recvRes );
  }
}
