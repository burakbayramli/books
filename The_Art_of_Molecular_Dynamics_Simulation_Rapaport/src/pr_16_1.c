
/* [[pr_16_1 - two-dimensional vibrating layer]] */


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
  real wv, wa, diam;
} Mol;

Mol *mol;
VecR region;
VecI initUcell;
real deltaT, rCut, kinEnergy, timeNow, velMag;
int moreCycles, nMol, stepAvg, stepCount, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
real basePos, baseVel, curPhase, fricDyn, fricStat, gravField,
   inertiaK, vibAmp, vibFreq;
int nBaseCycle;
int runId, stepSnap;

NameList nameList[] = {
  NameR (deltaT),
  NameR (fricDyn),
  NameR (gravField),
  NameI (initUcell),
  NameI (nebrTabFac),
  NameR (rNebrShell),
  NameI (runId),
  NameI (stepAvg),
  NameI (stepLimit),
  NameI (stepSnap),
  NameR (vibAmp),
  NameR (vibFreq),
};


int main (int argc, char **argv)
{
  GetNameList (argc, argv);
  PrintNameList (stdout);
  SetParams ();
  SetupJob ();
  moreCycles = 1;
  PutConfig ();
  while (moreCycles) {
    SingleStep ();
    if (stepCount % stepSnap == 0) PutConfig ();
    if (stepCount >= stepLimit) moreCycles = 0;
  }
}

void SetupFiles ()
{
  FILE *fp;

  strcpy (fileName[FL_SNAP], fileNameR[FL_SNAP]);
  fileName[FL_SNAP][0] = progId[0];
  fileName[FL_SNAP][1] = progId[1];
  fileName[FL_SNAP][2] = runId / 10 + CHAR_ZERO;
  fileName[FL_SNAP][3] = runId % 10 + CHAR_ZERO;
  fp = fopen (fileName[FL_SNAP], "w");
  fclose (fp);
}


void SingleStep ()
{
  ++ stepCount;
  timeNow = stepCount * deltaT;
  SetBase ();
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
  if (stepCount % stepAvg == 0) PrintSummary (stdout);
}

void SetupJob ()
{
  SetupFiles ();
  AllocArrays ();
  stepCount = 0;
  SetBase ();
  InitCoords ();
  InitVels ();
  InitAccels ();
  SetMolSizes ();
  nebrNow = 1;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  region.x = initUcell.x / 0.95;
  region.y = region.x;
  nMol = VProd (initUcell);
  VSCopy (cells, 1. / (rCut + rNebrShell), region);
  nebrTabMax = nebrTabFac * nMol;
  fricStat = 0.5;
  inertiaK = 0.125;
  velMag = 1.;
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
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS, zz;
  real dFac;
  int c, j1, j2, m1, m1x, m1y, m2, n, offset;

  VDiv (invWid, cells, region);
  for (n = nMol; n < nMol + VProd (cells); n ++) cellList[n] = -1;
  VZero (zz);
  DO_MOL {
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    if (! (VGe (cc, zz) && VLt (cc, cells)))
       ErrExit (ERR_OUTSIDE_REGION);
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
        VCellWrap (x);
        if (m2v.y >= cells.y) continue;
        m2 = VLinear (m2v, cells) + nMol;
        DO_CELL (j1, m1) {
          DO_CELL (j2, m2) {
            if (m1 != m2 || j2 < j1) {
              VSub (dr, mol[j1].r, mol[j2].r);
              VVSub (dr, shift);
              dFac = 0.5 * (mol[j1].diam + mol[j2].diam);
              if (VLenSq (dr) < Sqr (rCut * dFac + rNebrShell)) {
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
  VecR dr, dv;
  real aMass, dFac, drv, fcVal, ft, ftLim, rr, rri, rri3, rSep, vRel, ws, wt;
  int j1, j2, n;

  DO_MOL {
    VZero (mol[n].ra);
    mol[n].wa = 0.;
  }
  for (n = 0; n < nebrTabLen; n ++) {
    j1 = nebrTab[2 * n];
    j2 = nebrTab[2 * n + 1];
    dFac = 0.5 * (mol[j1].diam + mol[j2].diam);
    VSub (dr, mol[j1].r, mol[j2].r);
    VWrap (dr, x);
    rr = VLenSq (dr);
    if (rr < Sqr (rCut * dFac)) {
      rSep = sqrt (rr);
      rri = Sqr (dFac) / rr;
      rri3 = Cube (rri);
      fcVal = 48. * rri3 * (rri3 - 0.5) / rr;
      VSub (dv, mol[j1].rv, mol[j2].rv);
      drv = VDot (dr, dv);
      fcVal -= fricDyn * drv / rr;
      VVSAdd (mol[j1].ra, fcVal, dr);
      VVSAdd (mol[j2].ra, - fcVal, dr);
      VVSAdd (dv, - drv / rr, dr);
      ws = (mol[j1].diam * mol[j1].wv + mol[j2].diam * mol[j2].wv) /
         (mol[j1].diam + mol[j2].diam);
      dv.x += ws * dr.y;
      dv.y -= ws * dr.x;
      vRel = VLen (dv);
      ftLim = fricStat * fabs (fcVal) * rSep / vRel;
      ft = - Min (ftLim, fricDyn);
      VVSAdd (mol[j1].ra, ft, dv);
      VVSAdd (mol[j2].ra, - ft, dv);
      wt = ft * vRel;
      if (VCross (dr, dv) > 0.) wt = - wt;
      mol[j1].wa += wt;
      mol[j2].wa += wt;
    }
  }
  ComputeBdyForces ();
  DO_MOL {
    aMass = Sqr (mol[n].diam);
    VScale (mol[n].ra, 1. / aMass);
    mol[n].wa /= 2. * inertiaK * aMass * mol[n].diam;
    mol[n].ra.y -= gravField;
  }
}

void ComputeBdyForces ()
{
  VecR dr;
  real dFac, drv, fcVal, rr, rri, rri3;
  int n;

  DO_MOL {
    dFac = 0.5 * (mol[n].diam + 1.);
    dr.y = mol[n].r.y - 0.5 * region.y;
    if (fabs (dr.y) < rCut * dFac) {
      rr = Sqr (dr.y);
      rri = Sqr (dFac) / rr;
      rri3 = Cube (rri);
      fcVal = 48. * rri3 * (rri3 - 0.5) / rr;
      mol[n].ra.y += fcVal * dr.y;
    }
    dr.y = mol[n].r.y - basePos;
    if (fabs (dr.y) < rCut * dFac) {
      rr = Sqr (dr.y);
      rri = Sqr (dFac) / rr;
      rri3 = Cube (rri);
      fcVal = 48. * rri3 * (rri3 - 0.5) / rr;
      drv = dr.y * (mol[n].rv.y - baseVel);
      fcVal -= fricDyn * drv / rr;
      mol[n].ra.y += fcVal * dr.y;
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
      mol[n].wv += 0.5 * deltaT * mol[n].wa;
    }
  } else {
    DO_MOL {
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
      mol[n].wv += 0.5 * deltaT * mol[n].wa;
    }
  }
}

void ApplyBoundaryCond ()
{
  int n;

  DO_MOL VWrap (mol[n].r, x);
}

void EvalProps ()
{
  real vv, vvMax, vvSum;
  int n;

  vvSum = 0.;
  vvMax = 0.;
  DO_MOL {
    vv = VLenSq (mol[n].rv);
    if (vv > vvMax) vvMax = vv;
    vvSum += Sqr (mol[n].diam) * (vv + inertiaK *
       Sqr (mol[n].diam) * Sqr (mol[n].wv));
  }
  dispHi += sqrt (vvMax) * deltaT;
  if (dispHi > 0.5 * rNebrShell) nebrNow = 1;
  kinEnergy = 0.5 * vvSum / nMol;
}

void SetBase ()
{
  nBaseCycle = vibFreq * stepCount * deltaT;
  curPhase = vibFreq * stepCount * deltaT - nBaseCycle;
  basePos = - 0.5 * region.y + vibAmp * (1. - cos (2. * M_PI * curPhase));
  baseVel = 2. * M_PI * vibFreq * vibAmp * sin (2. * M_PI * curPhase);
}

void InitCoords ()
{
  VecR c, gap;
  int n, nx, ny;

  SetBase ();
  VDiv (gap, region, initUcell);
  gap.y = rCut;
  n = 0;
  for (ny = 0; ny < initUcell.y; ny ++) {
    c.y = ny * gap.y + basePos + 1.;
    for (nx = 0; nx < initUcell.x; nx ++) {
      c.x = (nx + 0.5) * gap.x - 0.5 * region.x;
      mol[n].r = c;
      ++ n;
    }
  }
}

void InitVels ()
{
  int n;

  DO_MOL {
    VRand (&mol[n].rv);
    VScale (mol[n].rv, velMag);
    mol[n].wv = 0.;
  }
}

void InitAccels ()
{
  int n;

  DO_MOL {
    VZero (mol[n].ra);
    mol[n].wa = 0.;
  }
}

void SetMolSizes ()
{
  int n;

  DO_MOL mol[n].diam = 1. - 0.15 * RandR ();
}

void PrintSummary (FILE *fp)
{
  fprintf (fp, "%5d %8.4f %7.4f",
     stepCount, timeNow, kinEnergy);
  fprintf (fp, "\n");
  fflush (fp);
}

#define SCALE_FAC  32767.

void PutConfig ()
{
  VecR w;
  int blockSize, fOk, n;
  short *rI;
  FILE *fp;

  fOk = 1;
  blockSize = (NDIM + 3) * sizeof (real) + 4 * sizeof (int) +
     nMol * NDIM * sizeof (short);
  if ((fp = fopen (fileName[FL_SNAP], "a")) != 0) {
    WriteF (blockSize);
    WriteF (basePos);
    WriteF (curPhase);
    WriteF (nBaseCycle);
    WriteF (nMol);
    WriteF (region);
    WriteF (stepCount);
    WriteF (timeNow);
    AllocMem (rI, NDIM * nMol, short);
    DO_MOL {
      VDiv (w, mol[n].r, region);
      VAddCon (w, w, 0.5);
      VScale (w, SCALE_FAC);
      VToLin (rI, NDIM * n, w);
    }
    WriteFN (rI, NDIM * nMol);
    free (rI);
    if (ferror (fp)) fOk = 0;
    fclose (fp);
  } else fOk = 0;
  if (! fOk) ErrExit (ERR_SNAP_WRITE);
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

