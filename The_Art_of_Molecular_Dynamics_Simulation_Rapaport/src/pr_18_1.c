
/* [[pr_18_1 - checkpoints]] */


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
int moreCycles, nMol, runId, stepAvg, stepCount, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
int doCheckpoint, newRun, recordSnap, stepCheckpoint;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameI (doCheckpoint),
  NameI (initUcell),
  NameI (nebrTabFac),
  NameI (recordSnap),
  NameR (rNebrShell),
  NameI (runId),
  NameI (stepAvg),
  NameI (stepCheckpoint),
  NameI (stepLimit),
  NameR (temperature),
};

void ProcInterrupt ()
{
  moreCycles = 0;
}

void SetupInterrupt ()
{
  signal (SIGUSR1, ProcInterrupt);
}

int main (int argc, char **argv)
{
  SetupInterrupt ();
  GetNameList (argc, argv);
  PrintNameList (stdout);
  SetParams ();
  SetupJob ();
  moreCycles = 1;
  while (moreCycles) {
    SingleStep ();
    if (doCheckpoint && stepCount % stepCheckpoint == 0)
       PutCheckpoint ();
    if (stepCount >= stepLimit) moreCycles = 0;
  }
  if (doCheckpoint) PutCheckpoint ();
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
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
}

void SetupJob ()
{
  SetupFiles ();
  AllocArrays ();
  if (newRun) {
    printf ("new run\n");
    stepCount = 0;
    InitCoords ();
    InitVels ();
    InitAccels ();
    AccumProps (0);
    if (doCheckpoint) {
      PutCheckpoint ();
      PutCheckpoint ();
    }
  } else {
    printf ("continued run\n");
    GetCheckpoint ();
  }
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
  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
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
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
      VVSAdd (mol[n].r, deltaT, mol[n].rv);
    }
  } else {
    DO_MOL VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
  }
}


void ApplyBoundaryCond ()
{
  int n;

  DO_MOL VWrapAll (mol[n].r);
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


void PutCheckpoint ()
{
  int fOk, fVal;
  FILE *fp;

  fOk = 0;
  if ((fp = fopen (fileName[FL_CKLAST], "r+")) != 0) {
    fVal = FL_CHECKA + FL_CHECKB - (fgetc (fp) - CHAR_ZERO);
    rewind (fp);
    fputc (CHAR_ZERO + fVal, fp);
    fclose (fp);
    fOk = 1;
  }
  if (fOk && (fp = fopen (fileName[fVal], "w")) != 0) {
    WriteF (kinEnergy);
    WriteF (stepCount);
    WriteF (timeNow);
    WriteF (totEnergy);
    WriteFN (mol, nMol);
    if (ferror (fp)) fOk = 0;
    fclose (fp);
  } else fOk = 0;
  if (! fOk) ErrExit (ERR_CHECKPT_WRITE);
}

void GetCheckpoint ()
{
  int fOk, fVal;
  FILE *fp;

  fOk = 0;
  if ((fp = fopen (fileName[FL_CKLAST], "r")) != 0) {
    fVal = fgetc (fp) - CHAR_ZERO;
    fclose (fp);
    fOk = 1;
  }
  if (fOk && (fp = fopen (fileName[fVal], "r")) != 0) {
    ReadF (kinEnergy);
    ReadF (stepCount);
    ReadF (timeNow);
    ReadF (totEnergy);
    ReadFN (mol, nMol);
    if (ferror (fp)) fOk = 0;
    fclose (fp);
  } else fOk = 0;
  if (! fOk) ErrExit (ERR_CHECKPT_READ);
}

void SetupFiles ()
{
  FILE *fp;
  int k;

  for (k = 0; k < sizeof (fileNameR) /
     sizeof (fileNameR[0]); k ++) {
    strcpy (fileName[k], fileNameR[k]);
    fileName[k][0] = progId[0];
    fileName[k][1] = progId[1];
    fileName[k][2] = runId / 10 + CHAR_ZERO;
    fileName[k][3] = runId % 10 + CHAR_ZERO;
  }
  if (! doCheckpoint) {
    newRun = 1;
  } else if ((fp = fopen (fileName[FL_CKLAST], "r")) != 0) {
    newRun = 0;
    fclose (fp);
  } else {
    newRun = 1;
    fp = fopen (fileName[FL_CHECKA], "w");
    fclose (fp);
    fp = fopen (fileName[FL_CHECKB], "w");
    fclose (fp);
    fp = fopen (fileName[FL_CKLAST], "w");
    fputc (CHAR_ZERO + FL_CHECKA, fp);
    fclose (fp);
  }
  if (newRun && recordSnap) {
    fp = fopen (fileName[FL_SNAP], "w");
    fclose (fp);
  }
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

