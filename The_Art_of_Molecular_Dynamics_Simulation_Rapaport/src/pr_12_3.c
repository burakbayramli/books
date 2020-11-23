
/* [[pr_12_3 - embedded-atom potential, collisions]] */


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
  real logRho;
  int fixed, inObj;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, rCut, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
real embedWt, rSwitch, splineA2, splineA3;
VecR diskInitPos;
real diskInitVel, initSep;
int nMolDisk, nMolWall;
int runId, stepSnap;

NameList nameList[] = {
  NameR (deltaT),
  NameR (diskInitVel),
  NameI (nebrTabFac),
  NameI (nMolDisk),
  NameI (nMolWall),
  NameR (rNebrShell),
  NameI (runId),
  NameI (stepAvg),
  NameI (stepLimit),
  NameI (stepSnap),
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
  LeapfrogStep (1);
  ApplyWallBoundaryCond ();
  if (nebrNow) {
    nebrNow = 0;
    dispHi = 0.;
    BuildNebrList ();
  }
  ComputeForces ();
  ZeroFixedAccels ();
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
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  AccumProps (0);
  nebrNow = 1;
}

void SetParams ()
{
  VecR t;

  EvalEamParams ();
  initSep = pow (2., 1./6.);
  region.x = 4. * nMolDisk * initSep;
  region.y = region.x;
  VSet (t, initSep, 0.5 * sqrt (3.) * initSep);
  VDiv (initUcell, region, t);
  initUcell.y += 2;
  VSet (diskInitPos, 0.5 * (- 0.5 * region.x - 0.5 * nMolWall * initSep), 0.);
  EvalMolCount ();
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
  real eDim, fcVal, rCutC, rr, rrCdi, rrCut, rrd, rri, rri3,
     rrSwitch, t, uVal;
  int j1, j2, n;

  eDim = NDIM * (NDIM + 1) * exp (1.);
  rCutC = pow (2., 1./6.);
  rrCut = Sqr (rCut);
  rrCdi = 1. / Sqr (rrCut - Sqr (rCutC));
  rrSwitch = Sqr (rSwitch);
  DO_MOL mol[n].logRho = 0.;
  for (n = 0; n < nebrTabLen; n ++) {
    j1 = nebrTab[2 * n];
    j2 = nebrTab[2 * n + 1];
    VSub (dr, mol[j1].r, mol[j2].r);
    VWrapAll (dr);
    rr = VLenSq (dr);
    if (rr < rrCut) {
      t = Sqr (rrCut - rr);
      mol[j1].logRho += t;
      mol[j2].logRho += t;
    }
  }
  DO_MOL {
    if (mol[n].logRho > 0.)
       mol[n].logRho = log ((rrCdi / eDim) * mol[n].logRho);
  }
  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  for (n = 0; n < nebrTabLen; n ++) {
    j1 = nebrTab[2 * n];
    j2 = nebrTab[2 * n + 1];
    VSub (dr, mol[j1].r, mol[j2].r);
    VWrapAll (dr);
    rr = VLenSq (dr);
    if (rr < rrCut) {
      rrd = rrCut - rr;
      if (rr < rrSwitch) {
        rri = 1. / rr;
        rri3 = Cube (rri);
        fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
        uVal = 4. * rri3 * (rri3 - 1.);
      } else {
        fcVal = (4. * splineA2 + 6. * splineA3 * rrd) * rrd;
        uVal = (splineA2 + splineA3 * rrd) * Sqr (rrd);
      }
      fcVal = embedWt * fcVal + (1. - embedWt) * 2. * rrCdi *
         (mol[j1].logRho + mol[j2].logRho + 2.) * rrd;
      VVSAdd (mol[j1].ra, fcVal, dr);
      VVSAdd (mol[j2].ra, - fcVal, dr);
      uSum += uVal;
    }
  }
  t = 0.;
  DO_MOL t += mol[n].logRho * exp (mol[n].logRho);
  uSum = embedWt * uSum + (1. - embedWt) * 0.5 * eDim * t;
}

void EvalEamParams ()
{
  real bb, p, pd, rr, rr3;

  rSwitch = pow (26. / 7., 1. / 6.);
  rr = Sqr (rSwitch);
  rr3 = Cube (rr);
  p = 4. * (1. / rr3 - 1.) / rr3;
  pd = - 48. * (1. / rr3 - 0.5) / (rSwitch * rr3);
  bb = 4. * (1. - sqrt (1. + 3. * p / (2. * rSwitch * pd)));
  splineA2 = (6. * p + bb * rSwitch * pd) / (2. * Sqr (bb * rr));
  splineA3 = - (4. * p + bb * rSwitch * pd) / (2. * Sqr (bb * rr) * bb * rr);
  rCut = rSwitch * sqrt (bb + 1.);
  embedWt = 0.3333;
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


void ApplyWallBoundaryCond ()
{
  real vSign;
  int n;

  DO_MOL {
    if (fabs (mol[n].r.x) >= 0.5 * region.x) {
      vSign = (mol[n].r.x > 0.) ? 1. : -1.;
      mol[n].r.x = 0.49999 * vSign * region.x;
      VScale (mol[n].rv, 0.1);
      if (mol[n].rv.x * vSign > 0.) mol[n].rv.x *= -1.;
    }
    if (fabs (mol[n].r.y) >= 0.5 * region.y) {
      vSign = (mol[n].r.y > 0.) ? 1. : -1.;
      mol[n].r.y = 0.49999 * vSign * region.y;
      VScale (mol[n].rv, 0.1);
      if (mol[n].rv.y * vSign > 0.) mol[n].rv.y *= -1.;
    }
  }
}

void ZeroFixedAccels ()
{
  int n;

  DO_MOL {
    if (mol[n].fixed) VZero (mol[n].ra);
  }
}

int CoordInRegion (int nx, int ny, VecR *pr)
{
  VecR c, dr;
  int regionCode;

  regionCode = -1;
  c.x = nx - initUcell.x / 2 + ((ny % 2 == 0) ? 0.5 : 0.);
  c.y = (ny - initUcell.y / 2) * 0.5 * sqrt (3.);
  VScale (c, initSep);
  if (fabs (c.x) < 0.5 * nMolWall * initSep && fabs (c.y) < 0.5 * region.y)
     regionCode = 1;
  else {
    VSub (dr, c, diskInitPos);
    if (VLenSq (dr) < Sqr (0.5 * nMolDisk * initSep)) regionCode = 0;
  }
  if (regionCode >= 0) *pr = c;
  return (regionCode);
}

void EvalMolCount ()
{
  VecR c;
  int nx, ny;

  nMol = 0;
  for (ny = 0; ny < initUcell.y; ny ++) {
    for (nx = 0; nx < initUcell.x; nx ++) {
      if (CoordInRegion (nx, ny, &c) >= 0) ++ nMol;
    }
  }
}

void InitCoords ()
{
  VecR c;
  real wyMax, wyMin;
  int n, nx, ny, p;

  n = 0;
  for (ny = 0; ny < initUcell.y; ny ++) {
    for (nx = 0; nx < initUcell.x; nx ++) {
      p = CoordInRegion (nx, ny, &c);
      if (p >= 0) {
        mol[n].r = c;
        mol[n].inObj = p;
        ++ n;
      }
    }
  }
  wyMin = 0.5 * region.y;
  wyMax = - wyMin;
  DO_MOL {
    if (mol[n].inObj == 1) {
      wyMin = Min (wyMin, mol[n].r.y);
      wyMax = Max (wyMax, mol[n].r.y);
    }
  }
  DO_MOL mol[n].fixed = (mol[n].inObj == 1 &&
     (mol[n].r.y == wyMin || mol[n].r.y == wyMax)) ? 1 : 0;
}

void InitVels ()
{
  int n;

  velMag = 0.1;
  VZero (vSum);
  DO_MOL {
    if (! mol[n].fixed) {
      VRand (&mol[n].rv);
      VScale (mol[n].rv, velMag);
      VVAdd (vSum, mol[n].rv);
    } else VZero (mol[n].rv);
  }
  DO_MOL {
    if (! mol[n].fixed) VVSAdd (mol[n].rv, - 1. / nMol, vSum);
    if (mol[n].inObj == 0) mol[n].rv.x += diskInitVel;
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


#define SCALE_FAC  32767.

void PutConfig ()
{
  VecR w;
  int blockSize, fOk, n;
  short *rI;
  FILE *fp;

  fOk = 1;
  blockSize = (NDIM + 1) * sizeof (real) + 3 * sizeof (int) +
     nMol * (NDIM + 1) * sizeof (short);
  if ((fp = fopen (fileName[FL_SNAP], "a")) != 0) {
    WriteF (blockSize);
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
    DO_MOL rI[n] = mol[n].inObj;
    WriteFN (rI, nMol);
    free (rI);
    if (ferror (fp)) fOk = 0;
    fclose (fp);
  } else fOk = 0;
  if (! fOk) ErrExit (ERR_SNAP_WRITE);
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

