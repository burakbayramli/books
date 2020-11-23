
/* [[pr_07_1 - pipe flow]] */


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


#include "in_mddefs.h"

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
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
VecI sizeHistGrid;
real **histGrid;
int countGrid, limitGrid, stepGrid;
real *profileT, *profileV, gravField;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameR (gravField),
  NameI (initUcell),
  NameI (limitGrid),
  NameI (nebrTabFac),
  NameR (rNebrShell),
  NameI (sizeHistGrid),
  NameI (stepAvg),
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
  ComputeExternalForce ();
  LeapfrogStep (2);
  EvalProps ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (stepCount >= stepEquil && (stepCount - stepEquil) % stepGrid == 0) {
    ++ countGrid;
    GridAverage (1);
    if (countGrid % limitGrid == 0) {
      GridAverage (2);
      EvalProfile ();
      PrintProfile (stdout);
      GridAverage (0);
    }
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
  nebrNow = 1;
  GridAverage (0);
  countGrid = 0;
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
  AllocMem (nebrTab, 2 * nebrTabMax, int);
  AllocMem2 (histGrid, NHIST, VProd (sizeHistGrid), real);
  AllocMem (profileT, sizeHistGrid.z, real);
  AllocMem (profileV, sizeHistGrid.z, real);
}


void BuildNebrList ()
{
  VecR dr, invWid, rs, shift;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrNebr;
  int c, j1, j2, m1, m1x, m1y, m1z, m2, n, offset;

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
  for (m1z = 0; m1z < cells.z; m1z ++) {
    for (m1y = 0; m1y < cells.y; m1y ++) {
      for (m1x = 0; m1x < cells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, cells) + nMol;
        for (offset = 0; offset < N_OFFSET; offset ++) {
          VAdd (m2v, m1v, vOff[offset]);
          VZero (shift);
          VCellWrap (x);
          VCellWrap (y);
          if (m2v.z < 0 || m2v.z >= cells.z) continue;
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


void ComputeExternalForce ()
{
  int n;

  DO_MOL mol[n].ra.x += gravField;
}


void LeapfrogStep (int part)
{
  int n;

  if (part == 1) {
    DO_MOL {
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
      VVSAdd (mol[n].r, deltaT, mol[n].rv);
    }
  } else {
    DO_MOL VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
  }
}


void ApplyBoundaryCond ()
{
  real vSign;
  int n;

  DO_MOL {
    VWrap (mol[n].r, x);
    VWrap (mol[n].r, y);
    vSign = 0.;
    if (mol[n].r.z >= 0.5 * region.z) vSign = 1.;
    else if (mol[n].r.z  < -0.5 * region.z) vSign = -1.;
    if (vSign != 0.) {
      mol[n].r.z  = 0.49999 * vSign * region.z;
      VRand (&mol[n].rv);
      VScale (mol[n].rv, velMag);
      if (mol[n].rv.z * vSign > 0.) mol[n].rv.z *= -1.;
    }
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
      histGrid[4][c] += mol[n].rv.z;
    }
  } else if (opCode == 2) {
    pSum = 0.;
    for (n = 0; n < hSize; n ++) {
      if (histGrid[0][n] > 0.) {
        for (j = 1; j < NHIST; j ++)
           histGrid[j][n] /= histGrid[0][n];
        VSet (va, histGrid[2][n], histGrid[3][n], histGrid[4][n]);
        histGrid[1][n] = (histGrid[1][n] - VLenSq (va)) / NDIM;
        pSum += histGrid[0][n];
      }
    }
    pSum /= hSize;
    for (n = 0; n < hSize; n ++) histGrid[0][n] /= pSum;
  }
}


void EvalProfile ()
{
  int k, n;

  for (n = 0; n < sizeHistGrid.z; n ++) {
    profileT[n] = 0.;
    profileV[n] = 0.;
  }
  for (n = 0; n < VProd (sizeHistGrid); n ++) {
    k = n / (sizeHistGrid.x * sizeHistGrid.y);
    profileT[k] += histGrid[1][n];
    profileV[k] += histGrid[2][n];
  }
  for (n = 0; n < sizeHistGrid.z; n ++) {
    profileT[n] /= sizeHistGrid.x * sizeHistGrid.y;
    profileV[n] /= sizeHistGrid.x * sizeHistGrid.y;
  }
}

void PrintProfile (FILE *fp)
{
  real zVal;
  int n;

  fprintf (fp, "V profile\n");
  for (n = 0; n < sizeHistGrid.z; n ++) {
    zVal = (n + 0.5) / sizeHistGrid.z;
    fprintf (fp, "%.2f %.3f\n", zVal, profileV[n]);
  }
  fprintf (fp, "T profile\n");
  for (n = 0; n < sizeHistGrid.z; n ++) {
    zVal = (n + 0.5) / sizeHistGrid.z;
    fprintf (fp, "%.2f %.3f\n", zVal, profileT[n]);
  }
  fflush (fp);
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

