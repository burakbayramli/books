//  Single iteration of the Jacobi scheme (GPU kernel function).
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

#include "defs.h"

// Basic kernel function which uses global memory only
//
__global__ void Jacobi( int Ni, int Nj, REAL *phi, REAL *phiOld )
{
  int i = blockDim.x * blockIdx.x + threadIdx.x;  // global index

  if (i>0 && i<(Ni-1))
  {
    int j = blockDim.y * blockIdx.y + threadIdx.y;
    if (j>0 && j<(Nj-1))
    {
      int ij  = i + j*Ni;
      phi[ij] = 0.25f*(phiOld[ij+1]+phiOld[ij-1]+phiOld[ij+Ni]+phiOld[ij-Ni]);
    }
  }
}

//*****************************************************************************

// Optimized kernel function which uses shared memory
//
__global__ void Jacobi_optim( int Ni, int Nj, int nThreadsX, int nThreadsY,
                              REAL *phi, REAL *phiOld )
{
  int i, j, ij, iLocal, jLocal, iLocalDum, jLocalDum;

  __shared__ REAL sharedMem[34][18];  // shared memory block including surrounding
                                      // dummy layer (1 deep) to cover the stencil
                                      // - sized as nThreadsX+2, nThreadsY+2

  i  = blockDim.x * blockIdx.x + threadIdx.x;  // global indexes
  j  = blockDim.y * blockIdx.y + threadIdx.y;
  ij = i + j*Ni;

  iLocal    = threadIdx.x;  // local indexes within grid block
  jLocal    = threadIdx.y;
  iLocalDum = iLocal + 1;   // including offset for dummy layer
  jLocalDum = jLocal + 1;

  if (iLocal == 0)  // copy left & right dummy layer (except at boundaries)
  {
    if (blockIdx.x > 0)
      sharedMem[iLocal][jLocalDum] = phiOld[ij-1];

	if (blockIdx.x < (gridDim.x-1))
      sharedMem[iLocalDum+nThreadsX][jLocalDum] = phiOld[ij+nThreadsX];
  }
  if (jLocal == 0)  // copy bottom & top dummy layer (except at boundaries)
  {
    if (blockIdx.y > 0)
	  sharedMem[iLocalDum][jLocal] = phiOld[ij-Ni];

	if (blockIdx.y < (gridDim.y-1))
      sharedMem[iLocalDum][jLocalDum+nThreadsY] = phiOld[ij+Ni*nThreadsY];
  }

  if (i<Ni && j<Nj)
    sharedMem[iLocalDum][jLocalDum] = phiOld[ij];  // copy interior elements

  __syncthreads();  // wait for all threads of the block to finish

  if (i>0 && i<(Ni-1))
  {
    if (j>0 && j<(Nj-1))
    {
      phi[ij] = 0.25f*(sharedMem[iLocalDum+1][jLocalDum]+sharedMem[iLocalDum-1][jLocalDum]+
                       sharedMem[iLocalDum][jLocalDum+1]+sharedMem[iLocalDum][jLocalDum-1]);
    }
  }
}
