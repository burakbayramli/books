
/* [[pr_17_2 - parallel processing using threads]] */


/*********************************************************************

  This program is copyright material accompanying the book
  "The Art of Molecular Dynamics Simulation", 2nd edition,
  by D. C. Rapaport, published by Cambridge University Press (2004).

  Copyright (C) 2004, 2011  D. C. Rapaport

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

**********************************************************************/


/* link with -lpthread */

#include "in_mddefs.h"

#include <pthread.h>

#define QUERY_THREAD()  ip = (long) tr
#define QUERY_STAGE  funcStage
#define THREAD_PROC_LOOP(tProc, fStage)                     \
   funcStage = fStage;                                      \
   for (ip = 1; ip < nThread; ip ++)                        \
      pthread_create (&pThread[ip], NULL,                   \
      tProc, (void *) ip);                                  \
   tProc ((void *) 0);                                      \
    for (ip = 1; ip < nThread; ip ++)                       \
      pthread_join (pThread[ip], NULL);
#define THREAD_SPLIT_LOOP(j, jMax)                          \
  for (j = ip * jMax / nThread;                             \
     j < (ip + 1) * jMax / nThread; j ++)
#define THREAD_LOOP  for (iq = 0; iq < nThread; iq ++)

typedef struct {
  VecR r, rv, ra;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
real kinEnInitSum;
int stepInitlzTemp;
pthread_t *pThread;
VecR **raP;
real *uSumP;
int **nebrTabP, *nebrTabLenP, nebrNow, nebrTabFac, nebrTabMax, funcStage, nThread;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (nebrTabFac),
  NameI (nThread),
  NameR (rNebrShell),
  NameI (stepAvg),
  NameI (stepEquil),
  NameI (stepInitlzTemp),
  NameI (stepLimit),
  NameR (temperature),
};


int main (int argc, char **argv)
{
  GetNameList (argc, argv);
  PrintNameList (stdout);
  SetParams ();
  SetupJob ();
  moreCycles = 1;
  while (moreCycles) {
    SingleStep ();
    if (stepCount >= stepLimit) moreCycles = 0;
  }
}


void SingleStep ()
{
  ++ stepCount;
  timeNow = stepCount * deltaT;
  LeapfrogStep (1);
  ApplyBoundaryCond ();
  if (nebrNow) {
    nebrNow = 0;
    dispHi = 0.;
    BuildNebrList ();
  }
  ComputeForces ();
  LeapfrogStep (2);
  EvalProps ();
  if (stepCount < stepEquil) AdjustInitTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
}

void SetupJob ()
{
  AllocArrays ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  AccumProps (0);
  kinEnInitSum = 0.;
  nebrNow = 1;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nMol = VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / (rCut + rNebrShell), region);
  nebrTabMax = nebrTabFac * nMol;
}

void AllocArrays ()
{
  int k;

  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (pThread, nThread, pthread_t);
  AllocMem (uSumP, nThread, real);
  AllocMem (nebrTabLenP, nThread, int);
  AllocMem2 (raP, nThread, nMol, VecR);
  AllocMem2 (nebrTabP, nThread, 2 * nebrTabMax / nThread, int);
}

void BuildNebrList ()
{
  int n;
  long ip;

  for (n = nMol; n < nMol + VProd (cells); n ++) cellList[n] = -1;
  THREAD_PROC_LOOP (BuildNebrListT, 1);
  THREAD_PROC_LOOP (BuildNebrListT, 2);
}

void *BuildNebrListT (void *tr)
{
  VecR dr, invWid, rs, shift;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrNebr;
  int c, j1, j2, m1, m1x, m1y, m1z, m2, n, offset;
  int ip;

  QUERY_THREAD ();
  switch (QUERY_STAGE) {
    case 1:
      VDiv (invWid, cells, region);
      DO_MOL {
        VSAdd (rs, mol[n].r, 0.5, region);
        VMul (cc, rs, invWid);
        if (cc.z % nThread == ip) {
          c = VLinear (cc, cells) + nMol;
          cellList[n] = cellList[c];
          cellList[c] = n;
        }
      }
      break;
    case 2:
      rrNebr = Sqr (rCut + rNebrShell);
      nebrTabLenP[ip] = 0;
      THREAD_SPLIT_LOOP (m1z, cells.z) {
        for (m1y = 0; m1y < cells.y; m1y ++) {
          for (m1x = 0; m1x < cells.x; m1x ++) {
            VSet (m1v, m1x, m1y, m1z);
            m1 = VLinear (m1v, cells) + nMol;
            for (offset = 0; offset < N_OFFSET; offset ++) {
              VAdd (m2v, m1v, vOff[offset]);
              VZero (shift);
              VCellWrapAll ();
              m2 = VLinear (m2v, cells) + nMol;
              DO_CELL (j1, m1) {
                DO_CELL (j2, m2) {
                  if (m1 != m2 || j2 < j1) {
                    VSub (dr, mol[j1].r, mol[j2].r);
                    VVSub (dr, shift);
                    if (VLenSq (dr) < rrNebr) {
                      if (nebrTabLenP[ip] * nThread >= nebrTabMax)
                         ErrExit (ERR_TOO_MANY_NEBRS);
                      nebrTabP[ip][2 * nebrTabLenP[ip]] = j1;
                      nebrTabP[ip][2 * nebrTabLenP[ip] + 1] = j2;
                      ++ nebrTabLenP[ip];
                    }
                  }
                }
              }
            }
          }
        }
      }
      break;
  }
  return (NULL);
}

void ComputeForces ()
{
  int iq;
  long ip;

  THREAD_PROC_LOOP (ComputeForcesT, 1);
  THREAD_PROC_LOOP (ComputeForcesT, 2);
  uSum = 0.;
  THREAD_LOOP uSum += uSumP[iq];
}

void *ComputeForcesT (void *tr)
{
  VecR dr;
  real fcVal, rr, rrCut, rri, rri3, uVal;
  int j1, j2, n;
  int ip, iq;

  QUERY_THREAD ();
  switch (QUERY_STAGE) {
    case 1:
      rrCut = Sqr (rCut);
      DO_MOL VZero (raP[ip][n]);
      uSumP[ip] = 0.;
      for (n = 0; n < nebrTabLenP[ip]; n ++) {
        j1 = nebrTabP[ip][2 * n];
        j2 = nebrTabP[ip][2 * n + 1];
        VSub (dr, mol[j1].r, mol[j2].r);
        VWrapAll (dr);
        rr = VLenSq (dr);
        if (rr < rrCut) {
          rri = 1. / rr;
          rri3 = Cube (rri);
          fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
          uVal = 4. * rri3 * (rri3 - 1.) + 1.;
          VVSAdd (raP[ip][j1], fcVal, dr);
          VVSAdd (raP[ip][j2], - fcVal, dr);
          uSumP[ip] += uVal;
        }
      }
      break;
    case 2:
      THREAD_SPLIT_LOOP (n, nMol) {
        VZero (mol[n].ra);
        THREAD_LOOP VVAdd (mol[n].ra, raP[iq][n]);
      }
      break;
  }
  return (NULL);
}

void LeapfrogStep (int part)
{
  long ip;

  THREAD_PROC_LOOP (LeapfrogStepT, part);
}

void *LeapfrogStepT (void *tr)
{
  int ip, n;

  QUERY_THREAD ();
  switch (QUERY_STAGE) {
    case 1:
      THREAD_SPLIT_LOOP (n, nMol) {
        VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
        VVSAdd (mol[n].r, deltaT, mol[n].rv);
      }
      break;
    case 2:
      THREAD_SPLIT_LOOP (n, nMol)
         VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
      break;
  }
  return (NULL);
}

void ApplyBoundaryCond ()
{
  long ip;

  THREAD_PROC_LOOP (ApplyBoundaryCondT, 0);
}

void *ApplyBoundaryCondT (void *tr)
{
  int ip, n;

  QUERY_THREAD ();
  THREAD_SPLIT_LOOP (n, nMol) VWrapAll (mol[n].r);
  return (NULL);
}


void AdjustInitTemp ()
{
  real vFac;
  int n;

  kinEnInitSum += kinEnergy.val;
  if (stepCount % stepInitlzTemp == 0) {
    kinEnInitSum /= stepInitlzTemp;
    vFac = velMag / sqrt (2. * kinEnInitSum);
    DO_MOL VScale (mol[n].rv, vFac);
    kinEnInitSum = 0.;
  }
}


void InitCoords ()
{
  VecR c, gap;
  int n, nx, ny, nz;

  VDiv (gap, region, initUcell);
  n = 0;
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.5, ny + 0.5, nz + 0.5);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, region);
        mol[n].r = c;
        ++ n;
      }
    }
  }
}


void InitVels ()
{
  int n;

  VZero (vSum);
  DO_MOL {
    VRand (&mol[n].rv);
    VScale (mol[n].rv, velMag);
    VVAdd (vSum, mol[n].rv);
  }
  DO_MOL VVSAdd (mol[n].rv, - 1. / nMol, vSum);
}


void InitAccels ()
{
  int n;

  DO_MOL VZero (mol[n].ra);
}


void EvalProps ()
{
  real vv, vvMax;
  int n;

  VZero (vSum);
  vvSum = 0.;
  vvMax = 0.;
  DO_MOL {
    VVAdd (vSum, mol[n].rv);
    vv = VLenSq (mol[n].rv);
    vvSum += vv;
    vvMax = Max (vvMax, vv);
  }
  dispHi += sqrt (vvMax) * deltaT;
  if (dispHi > 0.5 * rNebrShell) nebrNow = 1;
  kinEnergy.val = 0.5 * vvSum / nMol;
  totEnergy.val = kinEnergy.val + uSum / nMol;
}


void AccumProps (int icode)
{
  if (icode == 0) {
    PropZero (totEnergy);
    PropZero (kinEnergy);
  } else if (icode == 1) {
    PropAccum (totEnergy);
    PropAccum (kinEnergy);
  } else if (icode == 2) {
    PropAvg (totEnergy, stepAvg);
    PropAvg (kinEnergy, stepAvg);
  }
}


void PrintSummary (FILE *fp)
{
  fprintf (fp,
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f\n",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy));
  fflush (fp);
}


#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

