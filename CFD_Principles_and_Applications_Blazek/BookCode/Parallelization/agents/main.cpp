//  Solution of the Laplace equation by Jacobi iteration on
//  a uniform structured grid with Ni x Nj points. X-coordinate
//  is associated with the i-direction, y-coordinate with j.
//  This example employs Asynchronous Agents Library for
//  the parallelization.
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

#include <ctime>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <sstream>
#include "compute.h"
using namespace std;

int main( int argc, char** argv )
{
  int Ni = 512;        // number of nodes in i-(x-)direction
  int Nj = 512;        // number of nodes in j-(y-)direction
  int maxIter = 10000; // max. number of iterations
  int nProcs  = 4;     // number of parallel processes

  REAL *phi=NULL, *phiOld=NULL; // current and previous solution

  int     i, j, iproc, jbeg, jend, jblock;
  clock_t start, finish;

  // parse the command line

  ParseCommandLine( argc,argv,Ni,Nj,maxIter,nProcs );
  cout << "\nNi=" << Ni
       << ", Nj=" << Nj
       << ", maxIter=" << maxIter
       << ", nProcs=" << nProcs
       << endl;

  // allocate memory

  phi    = new REAL[Ni*Nj];
  phiOld = new REAL[Ni*Nj];

  // initialize solution

  for (j=0; j<Nj; j++)
  {
    for (i=0; i<Ni; i++)
    {
      phi[i+Ni*j] = phiOld[i+Ni*j] = 0.0f;
    }
  }

  // set boundary conditions at j=0, j=Nj-1

  for (i=0; i<Ni; i++)
  {
    phi[i+Ni* 0    ] = phiOld[i+Ni* 0    ] = 5.0f;
    phi[i+Ni*(Nj-1)] = phiOld[i+Ni*(Nj-1)] = 5.0f;
  }

  // set boundary conditions at i=0, i=Ni-1

  for (j=1; j<(Nj-1); j++)
  {
    phi[0   +Ni*j] = phiOld[0   +Ni*j] = 5.0f;
    phi[Ni-1+Ni*j] = phiOld[Ni-1+Ni*j] = 5.0f;
  }

  // allocate send and receive buffers; create the agents
  // (note that grid gets split up in the y/j-direction)

  unbounded_buffer<REAL> sendResid, recvResid; // block-wise residuals and total residual from task 0

  Compute *agents[MAXPROCS];
  jblock = (Nj+nProcs-3)/nProcs;
  jbeg   = 1;
  jend   = 1;
  for (iproc=0; iproc<nProcs; iproc++)
  {
    jend = jbeg + jblock - 1;
    jend = min(jend,Nj-2);
    if (iproc == 0)
      agents[iproc] = new Compute( nProcs,iproc,recvResid,sendResid,
                                   Ni,Nj,jbeg,jend,phi,phiOld,maxIter );
    else
      agents[iproc] = new Compute( nProcs,iproc,sendResid,recvResid,
                                   Ni,Nj,jbeg,jend,phi,phiOld,maxIter );
    jbeg = jend + 1;
  }

  // compute the solution in parallel

  cout << scientific << setprecision(4) << right << setfill(' ');
  cout << "\nConvergence history:" << endl;
  start = clock();

  for (iproc=0; iproc<nProcs; iproc++)
  {
    agents[iproc]->start();
  }

  for (iproc=0; iproc<nProcs; iproc++)
  {
    agent::wait( agents[iproc] );
  }

  finish = clock();
  cout << "\nTime to solution: " << fixed << setprecision(1)
       << ((double)(finish-start))/CLOCKS_PER_SEC
       << " [sec]" << endl;

  // store the solution in a file

  ofstream stream( "plot.v2d",ios::binary | ios::trunc );

  stream << "Solution of Laplace Equation" << endl
         << "1" << endl
         << "Field Plot" << endl
         << "1 3" << endl
         << "x" << endl
         << "y" << endl
         << "f" << endl
         << Ni << " " << Nj << endl
         << "0 0 0" << endl
         << "Structured" << endl;
  stream << scientific;

  for (j=0; j<Nj; j++)
  {
    for (i=0; i<Ni; i++)
    {
      stream << i << " " << j << " "<< phi[i+Ni*j] << endl;
    }
  }

  stream.close();

  return EXIT_SUCCESS;
}

//*****************************************************************************

// Parses the command line
//
void ParseCommandLine( int argc, char **argv, int &Ni, int &Nj,
                       int &maxIter, int &nProcs )
{
  if (argc >= 1)
  {
    for (int i=1; i<argc; i++)
    {
      bool firstArgIsParam = false;
      int stringStart = 0;

      while (argv[i][stringStart] == '-') stringStart++;
      char *stringArgv = &argv[i][stringStart];

      if (!STRNCASECMP( stringArgv,"Ni=",3 ))
      {
        firstArgIsParam = true;
        Ni = atoi(&stringArgv[3]);
        continue;
      }
      if (!STRNCASECMP( stringArgv,"Nj=",3 ))
      {
        firstArgIsParam = true;
        Nj = atoi(&stringArgv[3]);
        continue;
      }
      if (!STRNCASECMP( stringArgv,"maxIter=",8 ))
      {
        firstArgIsParam = true;
        maxIter = atoi(&stringArgv[8]);
        continue;
      }
      if (!STRNCASECMP( stringArgv,"nProcs=",7 ))
      {
        firstArgIsParam = true;
        nProcs = atoi(&stringArgv[7]);
        nProcs = min(nProcs,MAXPROCS);
        continue;
      }
      if (!firstArgIsParam || !STRNCASECMP( stringArgv,"h",1 ))
      {
        cout << "Usage:\n\n"
             << "jacobi [-h] [-Ni=] [-Nj=] [-maxIter=] [-nProcs=]"
             << endl;
        exit(0);
      }
    }
  }
}