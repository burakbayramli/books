//  Solution of the Laplace equation by Jacobi iteration on
//  a uniform structured grid with Ni x Nj nodes. X-coordinate
//  is associated with the i-direction, y-coordinate with j.
//  This example employs Nvidia's CUDA for the parallelization.
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "defs.h"

int main( int argc, char** argv )
{
  int Ni = 512;        // number of nodes in i-(x-)direction
  int Nj = 512;        // number of nodes in j-(y-)direction
  int maxIter = 10000; // max. number of iterations
  int nThreadsX = 32;  // number of threads in x-direction
  int nThreadsY = 16;  // number of threads in y-direction

  REAL *phi=NULL;                     // solution on the host
  REAL *phiDev=NULL, *phiDevOld=NULL; // current and previous solution on the GPU

  float       elapsedTime;
  int         i, j;
  cudaEvent_t start, stop;
  FILE        *fp;

  // parse the command line

  ParseCommandLine( argc,argv,&Ni,&Nj,&maxIter,&nThreadsX,&nThreadsY );
  printf("\nNi=%d, Nj=%d, maxIter=%d, nThreadsX=%d, nThreadsY=%d\n",Ni,Nj,maxIter,
         nThreadsX,nThreadsY);

  // memory for the result (on CPU)

  phi = (REAL*) calloc( Ni*Nj,sizeof(REAL) );

  // set boundary conditions at j=0, j=Nj-1

  for (i=0; i<Ni; i++)
  {
    phi[i+Ni* 0    ] = 5.0f;
    phi[i+Ni*(Nj-1)] = 5.0f;
  }

  // set boundary conditions at i=0, i=Ni-1

  for (j=1; j<(Nj-1); j++)
  {
    phi[0   +Ni*j] = 5.0f;
    phi[Ni-1+Ni*j] = 5.0f;
  }

  // initialize GPU stuff

  cudaError_t cudaStatus = Initialize( Ni,Nj,phi,&phiDev,&phiDevOld );
  if (cudaStatus != cudaSuccess)
  {
    printf("Initialization failed!\n");
    free( phi );
    Uninitialize( &phiDev,&phiDevOld );
    return 1;
  }

  dim3 threadsPerBlock( nThreadsX,nThreadsY );
  dim3 blocksPerGrid( (Ni+threadsPerBlock.x-1)/threadsPerBlock.x,
                      (Nj+threadsPerBlock.y-1)/threadsPerBlock.y );

  // perform iterations

  printf("Performing iterations ...\n");
  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  cudaEventRecord( start,NULL );

  for (int iter=1; iter<=maxIter; iter++)
  {
    //Jacobi<<<blocksPerGrid,threadsPerBlock>>>( Ni,Nj,phiDev,phiDevOld );

    Jacobi_optim<<<blocksPerGrid,threadsPerBlock>>>( Ni,Nj,nThreadsX,nThreadsY,
                                                     phiDev,phiDevOld );

    REAL *tmp = phiDevOld;
    phiDevOld = phiDev;
    phiDev    = tmp;
  }

  cudaEventRecord( stop,NULL );
  cudaEventSynchronize( stop );
  cudaEventElapsedTime( &elapsedTime,start,stop );
  printf("GPU Time: %g sec", elapsedTime/1000.0f);

  // copy solution to host's memory

  cudaStatus = cudaMemcpy( phi,phiDev,(Ni*Nj)*sizeof(REAL),cudaMemcpyDeviceToHost );
  if (cudaStatus != cudaSuccess)
  {
    printf("cudaMemcpy from PHIDEV to PHI failed!\n");
    return cudaStatus;
  }

  // free memory and reset GPU

  Uninitialize( &phiDev,&phiDevOld );

  // store solution in a plot file

  fp = fopen("plot.v2d","wt");
  fprintf(fp,"Solution of Laplace Equation\n");
  fprintf(fp,"1\n");
  fprintf(fp,"Field Plot\n");
  fprintf(fp,"1 3\n");
  fprintf(fp,"x\ny\nf\n");
  fprintf(fp,"%i %i\n",Ni,Nj);
  fprintf(fp,"0 0 0\n");
  fprintf(fp,"Structured\n");
  for (j=0; j<Nj; j++)
  {
    for (i=0; i<Ni; i++)
    {
      fprintf(fp,"%d %d %14.6e\n",i,j,phi[i+Ni*j]);
    }
  }
  fclose(fp);

  // finish

  free( phi );
  printf("\nFinished!\n");
  return EXIT_SUCCESS;
}

//*****************************************************************************

// Parses the command line
//
void ParseCommandLine( int argc, char**argv, int *Ni, int *Nj,
                       int *maxIter, int *nThreadsX, int *nThreadsY )
{
  if (argc >= 1)
  {
    for (int i=1; i<argc; i++)
    {
      int bFirstArgIsParam = false;
      int stringStart = 0;

      while (argv[i][stringStart] == '-') stringStart++;
      char *stringArgv = &argv[i][stringStart];

      if (!STRNCASECMP( stringArgv,"Ni=",3 ))
      {
        bFirstArgIsParam = true;
        *Ni = atoi(&stringArgv[3]);
        continue;
      }
      if (!STRNCASECMP( stringArgv,"Nj=",3 ))
      {
        bFirstArgIsParam = true;
        *Nj = atoi(&stringArgv[3]);
        continue;
      }
      if (!STRNCASECMP( stringArgv,"maxIter=",8 ))
      {
        bFirstArgIsParam = true;
        *maxIter = atoi(&stringArgv[8]);
        continue;
      }
      if (!STRNCASECMP( stringArgv,"nThreadsX=",10 ))
      {
        bFirstArgIsParam = true;
        *nThreadsX = atoi(&stringArgv[10]);
        continue;
      }
      if (!STRNCASECMP( stringArgv,"nThreadsY=",10 ))
      {
        bFirstArgIsParam = true;
        *nThreadsY = atoi(&stringArgv[10]);
        continue;
      }
      if (!bFirstArgIsParam || !STRNCASECMP( stringArgv,"h",1 ))
      {
        printf("Usage:\n\n"
               "jacobi [-h] [-Ni=] [-Nj=] [-maxIter=] [-nThreadsX=] [-nThreadsY=]\n");
        exit(0);
      }
    }
  }
}

//*****************************************************************************

// Initializes CUDA device and memory
//
cudaError_t Initialize( int Ni, int Nj, REAL *phiHost, REAL **phi, REAL **phiOld )
{
  cudaError_t cudaStatus;
  unsigned int size = Ni * Nj * sizeof(REAL);

  // choose which GPU to run on, change this on a multi-GPU system
  cudaStatus = cudaSetDevice(0);
  if (cudaStatus != cudaSuccess)
  {
    printf("cudaSetDevice failed! Do you have a CUDA-capable GPU installed?\n");
    return cudaStatus;
  }

  // allocate GPU buffers
  cudaStatus = cudaMalloc( (void**)&(*phi),size );
  if (cudaStatus != cudaSuccess)
  {
    fprintf(stderr, "cudaMalloc for PHI failed!\n");
    return cudaStatus;
  }

  cudaStatus = cudaMalloc( (void**)&(*phiOld),size );
  if (cudaStatus != cudaSuccess)
  {
    printf("cudaMalloc for PHIOLD failed!\n");
    return cudaStatus;
  }

  // initialize GPU buffers

  cudaStatus = cudaMemcpy( *phi,phiHost,size,cudaMemcpyHostToDevice );
  if (cudaStatus != cudaSuccess)
  {
    printf("cudaMemcpy from PHIHOST to PHI failed!\n");
    return cudaStatus;
  }

  cudaStatus = cudaMemcpy( *phiOld,phiHost,size,cudaMemcpyHostToDevice );
  if (cudaStatus != cudaSuccess)
  {
    printf("cudaMemcpy from PHIHOST to PHIOLD failed!\n");
    return cudaStatus;
  }

  //cudaDeviceSetSharedMemConfig( cudaSharedMemBankSizeEightByte );
  //cudaDeviceSetCacheConfig( cudaFuncCachePreferShared );
  
  //cudaComputeModeExclusive

  return cudaSuccess;
}

//*****************************************************************************

// Cleans up CUDA device and memory
//
void Uninitialize( REAL **phi, REAL **phiOld )
{
  cudaFree( *phi );
  cudaFree( *phiOld );

  cudaError_t cudaStatus = cudaDeviceReset();
  if (cudaStatus != cudaSuccess) printf("cudaDeviceReset failed!\n");
}
