
/* [[pr_15_2 - soft-disk obstructed flow]] */


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


#define NDIM  2

#include "in_mddefs.h"

typedef struct {
  VecR r, rv, ra;
  int fixed;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit, runId;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
VecR obsPos;
VecI sizeHistGrid;
real **histGrid, bdyStripWidth, flowSpeed, obsSize;
int countGrid, limitGrid, nFixedMol, nFreeMol, snapNumber, stepDrive,
   stepGrid;

NameList nameList[] = {
  NameR (bdyStripWidth),
  NameR (deltaT),
  NameR (density),
  NameR (flowSpeed),
  NameI (initUcell),
  NameI (limitGrid),
  NameI (nebrTabFac),
  NameR (obsPos),
  NameR (obsSize),
  NameR (rNebrShell),
  NameI (runId),
  NameI (sizeHistGrid),
  NameI (stepAvg),
  NameI (stepDrive),
  NameI (stepEquil),
  NameI (stepGrid),
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
  if (stepCount % stepDrive == 0) DriveFlow ();
  EvalProps ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (stepCount >= stepEquil && (stepCount - stepEquil) %
     stepGrid == 0) {
    GridAverage (1);
    ++ countGrid;
    if (countGrid % limitGrid == 0) {
      GridAverage (2);
      ++ snapNumber;
      PutGridAverage ();
      GridAverage (0);
    }
  }
}

void SetupJob ()
{
  SetupFiles ();
  AllocArrays ();
  SetMolType ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  AccumProps (0);
  nebrNow = 1;
  GridAverage (0);
  countGrid = 0;
  snapNumber = 0;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / sqrt (density), initUcell);
  EvalMolCount ();
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / (rCut + rNebrShell), region);
  nebrTabMax = nebrTabFac * nMol;
}

void AllocArrays ()
{
  int k;

  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
  AllocMem2 (histGrid, NHIST, VProd (sizeHistGrid), real);
}


void BuildNebrList ()
{
  VecR dr, invWid, rs, shift;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrNebr;
  int c, j1, j2, m1, m1x, m1y, m2, n, offset;

  rrNebr = Sqr (rCut + rNebrShell);
  VDiv (invWid, cells, region);
  for (n = nMol; n < nMol + VProd (cells); n ++) cellList[n] = -1;
  DO_MOL {
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nMol;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
  nebrTabLen = 0;
  for (m1y = 0; m1y < cells.y; m1y ++) {
    for (m1x = 0; m1x < cells.x; m1x ++) {
      VSet (m1v, m1x, m1y);
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
                if (nebrTabLen >= nebrTabMax)
                   ErrExit (ERR_TOO_MANY_NEBRS);
                nebrTab[2 * nebrTabLen] = j1;
                nebrTab[2 * nebrTabLen + 1] = j2;
                ++ nebrTabLen;
              }
            }
          }
        }
      }
    }
  }
}


void ComputeForces ()
{
  VecR dr;
  real fcVal, rr, rrCut, rri, rri3, uVal;
  int j1, j2, n;

  rrCut = Sqr (rCut);
  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  for (n = 0; n < nebrTabLen; n ++) {
    j1 = nebrTab[2 * n];
    j2 = nebrTab[2 * n + 1];
    VSub (dr, mol[j1].r, mol[j2].r);
    VWrapAll (dr);
    rr = VLenSq (dr);
    if (rr < rrCut) {
      rri = 1. / rr;
      rri3 = Cube (rri);
      fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
      uVal = 4. * rri3 * (rri3 - 1.) + 1.;
      VVSAdd (mol[j1].ra, fcVal, dr);
      VVSAdd (mol[j2].ra, - fcVal, dr);
      uSum += uVal;
    }
  }
}


void LeapfrogStep (int part)
{
  int n;

  if (part == 1) {
    DO_MOL {
      if (mol[n].fixed) continue;
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
      VVSAdd (mol[n].r, deltaT, mol[n].rv);
    }
  } else {
    DO_MOL {
      if (mol[n].fixed) continue;
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
    }
  }
}


void ApplyBoundaryCond ()
{
  int n;

  DO_MOL VWrapAll (mol[n].r);
}


void GridAverage (int opCode)
{
  VecR invWid, rs, va;
  VecI cc;
  real pSum;
  int c, hSize, j, n;

  hSize = VProd (sizeHistGrid);
  if (opCode == 0) {
    for (j = 0; j < NHIST; j ++) {
      for (n = 0; n < hSize; n ++) histGrid[j][n] = 0.;
    }
  } else if (opCode == 1) {
    VDiv (invWid, sizeHistGrid, region);
    DO_MOL {
      VSAdd (rs, mol[n].r, 0.5, region);
      VMul (cc, rs, invWid);
      c = VLinear (cc, sizeHistGrid);
      ++ histGrid[0][c];
      histGrid[1][c] += VLenSq (mol[n].rv);
      histGrid[2][c] += mol[n].rv.x;
      histGrid[3][c] += mol[n].rv.y;
    }
  } else if (opCode == 2) {
    pSum = 0.;
    for (n = 0; n < hSize; n ++) {
      if (histGrid[0][n] > 0.) {
        for (j = 1; j < NHIST; j ++)
           histGrid[j][n] /= histGrid[0][n];
        VSet (va, histGrid[2][n], histGrid[3][n]);
        histGrid[1][n] = (histGrid[1][n] - VLenSq (va)) / NDIM;
        pSum += histGrid[0][n];
      }
    }
    pSum /= hSize;
    for (n = 0; n < hSize; n ++) histGrid[0][n] /= pSum;
  }
}


#define SCALE_FAC  32767.

void PutGridAverage ()
{
  real histMax[NHIST], w;
  int blockSize, fOk, hSize, j, n;
  short *hI;
  FILE *fp;

  hSize = VProd (sizeHistGrid);
  for (j = 0; j < NHIST; j ++) {
    histMax[j] = 0.;
    for (n = 0; n < hSize; n ++) {
      w = fabs (histGrid[j][n]);
      histMax[j] = Max (histMax[j], w);
    }
    if (histMax[j] == 0.) histMax[j] = 1.;
  }
  fOk = 1;
  blockSize = (NHIST + 1) * sizeof (real) + sizeof (VecR) +
     (NDIM + 3) * sizeof (int) + NHIST * hSize * sizeof (short);
  if ((fp = fopen (fileName[FL_SNAP], "a")) != 0) {
    WriteF (blockSize);
    WriteF (histMax);
    WriteF (region);
    WriteF (runId);
    WriteF (sizeHistGrid);
    WriteF (snapNumber);
    WriteF (timeNow);
    AllocMem (hI, hSize, short);
    for (j = 0; j < NHIST; j ++) {
      for (n = 0; n < hSize; n ++)
         hI[n] = SCALE_FAC * histGrid[j][n] / histMax[j];
      WriteFN (hI, hSize);
    }
    free (hI);
    if (ferror (fp)) fOk = 0;
    fclose (fp);
  } else fOk = 0;
  if (! fOk) ErrExit (ERR_SNAP_WRITE);
}

void SetupFiles ()
{
  strcpy (fileName[FL_SNAP], fileNameR[FL_SNAP]);
  fileName[FL_SNAP][0] = progId[0];
  fileName[FL_SNAP][1] = progId[1];
  fileName[FL_SNAP][2] = runId / 10 + CHAR_ZERO;
  fileName[FL_SNAP][3] = runId % 10 + CHAR_ZERO;
}


void DriveFlow ()
{
  int n;

  DO_MOL {
    if (! mol[n].fixed && fabs (mol[n].r.x) >
       0.5 * region.x - bdyStripWidth) {
      VRand (&mol[n].rv);
      VScale (mol[n].rv, velMag);
      mol[n].rv.x += flowSpeed;
    }
  }
}

void EvalMolCount ()
{
  VecR c, gap;
  int nx, ny;

  nFixedMol = M_PI * obsSize * region.y / rCut;
  VDiv (gap, region, initUcell);
  nFreeMol = 0;
  for (ny = 0; ny < initUcell.y; ny ++) {
    for (nx = 0; nx < initUcell.x; nx ++) {
      VSet (c, nx + 0.5, ny + 0.5);
      VMul (c, c, gap);
      VVSAdd (c, -0.5, region);
      if (OutsideObs (&c)) ++ nFreeMol;
    }
  }
  nMol = nFixedMol + nFreeMol;
}

int OutsideObs (VecR *p)
{
  int outside;

  outside = (Sqr (p->x - obsPos.x * region.x) + Sqr (p->y -
     obsPos.y * region.y) > Sqr (0.5 * obsSize * region.y + rCut));
  return (outside);
}

void SetMolType ()
{
  int n;

  for (n = 0; n < nFixedMol; n ++) mol[n].fixed = 1;
  for (n = nFixedMol; n < nMol; n ++) mol[n].fixed = 0;
}

void InitCoords ()
{
  VecR c, gap, w;
  real ang;
  int n, nx, ny;

  VDiv (gap, region, initUcell);
  VMul (w, obsPos, region);
  for (n = 0; n < nFixedMol; n ++) {
    ang = 2. * M_PI * n / nFixedMol;
    VSet (c, cos (ang), sin (ang));
    VSAdd (mol[n].r, w, 0.5 * obsSize * region.y, c);
  }
  n = nFixedMol;
  for (ny = 0; ny < initUcell.y; ny ++) {
    for (nx = 0; nx < initUcell.x; nx ++) {
      VSet (c, nx + 0.5, ny + 0.5);
      VMul (c, c, gap);
      VVSAdd (c, -0.5, region);
      if (OutsideObs (&c)) {
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
    if (! mol[n].fixed) {
      VRand (&mol[n].rv);
      VScale (mol[n].rv, velMag);
      VVAdd (vSum, mol[n].rv);
    } else VZero (mol[n].rv);
  }
  DO_MOL {
    if (! mol[n].fixed) {
      VVSAdd (mol[n].rv, - 1. / nFreeMol, vSum);
      mol[n].rv.x += flowSpeed;
    }
  }
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

