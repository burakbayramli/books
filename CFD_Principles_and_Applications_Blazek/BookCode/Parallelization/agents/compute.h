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

#include <agents.h>
#include <cstdlib>
#include <cmath>
#include <iostream>
#include <iomanip>
using namespace concurrency;

// floating point type (SGLPREC=single precision, otherwise double)
#ifdef SGLPREC
  typedef float  REAL;
  #define SQRT   sqrtf
#else
  typedef double REAL;
  #define SQRT   sqrt
#endif

#define MAXPROCS 16  // max. number of parallel processes 

#ifndef STRNCASECMP
#ifdef _WIN32
#define STRNCASECMP _strnicmp
#else
#define STRNCASECMP strncasecmp
#endif
#endif

// Class for performing Jacobi iterations in parallel
class Compute : public agent
{
public:
  explicit Compute( int _nProcs, int _task,
                    ITarget<REAL> &_sendRes, ISource<REAL> &_recvRes,
                    int _Ni, int _Nj, int _jbeg, int _jend,
                    REAL _phi[], REAL _phiOld[], int _maxIter )
    : nProcs(_nProcs)    // total number of parallel processes
    , task(_task)        // number of my process (task=0 is the master process)
    , sendRes(_sendRes)  // send buffer for block-wise residuals
    , recvRes(_recvRes)  // receive buffer for block-wise residuals
    , Ni(_Ni)            // number of nodes in x-(i-)direction
    , Nj(_Nj)            // number of nodes in y-(j-)direction
    , jbeg(_jbeg)        // start index of the y/j-block
    , jend(_jend)        // end index
    , phi(_phi)          // current solution
    , phiOld(_phiOld)    // solution from the previous iteration
    , maxIter(_maxIter)  // max. number of iterations
  {
  }

protected:
  void run()
  {
    using namespace std;
    int  iter;
    REAL resid, resid0;

    if (task == 0)
      cout << scientific << setprecision(4) << right << setfill(' ');

    for (iter=1; iter<=maxIter; iter++)
    {
      Jacobi( resid );
      if (iter == 1) resid0 = resid;
      if (task == 0)
      {
        if (iter==1 || iter%1000 == 0)
          cout << setw(5)  << iter
               << setw(14) << resid/resid0 << endl;
      }
    }

    done();
  }

private:
  ITarget<REAL> &sendRes;
  ISource<REAL> &recvRes;
  int  nProcs, task, Ni, Nj, jbeg, jend, maxIter;
  REAL *phi, *phiOld;

  // single Jacobi iteration
  void Jacobi( REAL &resid );
};

// helper function

void ParseCommandLine( int argc, char **argv, int &Ni, int &Nj, int &maxIter, int &nProcs );

#endif // COMPUTE_H_INCLUDED