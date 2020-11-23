
/* [[pr_09_1 - flexible chain]] */


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
  int inChain;
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
int stepAdjustTemp;
VecI initUchain;
int chainLen, nChain;
real bbDistSq, bondLim, eeDistSq, gMomRatio1, gMomRatio2, radGyrSq;
int countChainProps, limitChainProps, stepChainProps;

NameList nameList[] = {
  NameR (bondLim),
  NameI (chainLen),
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (initUchain),
  NameI (limitChainProps),
  NameI (nebrTabFac),
  NameR (rNebrShell),
  NameI (stepAdjustTemp),
  NameI (stepAvg),
  NameI (stepChainProps),
  NameI (stepEquil),
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
  ComputeChainBondForces ();
  ApplyThermostat ();
  LeapfrogStep (2);
  EvalProps ();
  if (stepCount % stepAdjustTemp == 0) AdjustTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (stepCount >= stepEquil && (stepCount - stepEquil) %
     stepChainProps == 0) EvalChainProps ();
}

void SetupJob ()
{
  AllocArrays ();
  AssignToChain ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  AccumProps (0);
  nebrNow = 1;
  countChainProps = 0;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nChain = 2 * VProd (initUchain);
  if (nChain == 2) nChain = 1;
  nMol = VProd (initUcell) + nChain * chainLen;
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
              if ((m1 != m2 || j2 < j1) && (mol[j1].inChain == -1 ||
                 mol[j1].inChain != mol[j2].inChain ||
                 abs (j1 - j2) > 1)) {
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


void ComputeChainBondForces ()
{
  VecR dr;
  real fcVal, rr, rrCut, rri, rri3, uVal, w;
  int i, j1, j2, n;

  rrCut = Sqr (rCut);
  for (n = 0; n < nChain; n ++) {
    for (i = 0; i < chainLen - 1; i ++) {
      j1 = n * chainLen + i;
      j2 = j1 + 1;
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
      w = 1. - bondLim / sqrt (rr);
      if (w > 0.) ErrExit (ERR_BOND_SNAPPED);
      rr *= Sqr (w);
      if (rr < rrCut) {
        rri = 1. / rr;
        rri3 = Cube (rri);
        fcVal = 48. * w * rri3 * (rri3 - 0.5) * rri;
        uVal = 4. * rri3 * (rri3 - 1.) + 1.;
        VVSAdd (mol[j1].ra, fcVal, dr);
        VVSAdd (mol[j2].ra, - fcVal, dr);
        uSum += uVal;
      }
    }
  }
}


void ApplyThermostat ()
{
  VecR vt;
  real s1, s2, vFac;
  int n;

  s1 = s2 = 0.;
  DO_MOL {
    VSAdd (vt, mol[n].rv, 0.5 * deltaT, mol[n].ra);
    s1 += VDot (vt, mol[n].ra);
    s2 += VLenSq (vt);
  }
  vFac = - s1 / s2;
  DO_MOL {
    VSAdd (vt, mol[n].rv, 0.5 * deltaT, mol[n].ra);
    VVSAdd (mol[n].ra, vFac, vt);
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


void AdjustTemp ()
{
  real vFac;
  int n;

  vvSum = 0.;
  DO_MOL vvSum += VLenSq (mol[n].rv);
  vFac = velMag / sqrt (vvSum / nMol);
  DO_MOL VScale (mol[n].rv, vFac);
}


void InitCoords ()
{
  VecR c, dr, gap;
  real by, bz;
  int i, j, m, n, nx, ny, nz;

  by = rCut * cos (M_PI / 4.);
  bz = rCut * sin (M_PI / 4.);
  n = 0;
  if (nChain == 1) {
    for (m = 0; m < chainLen; m ++) {
      VSet (mol[n].r, 0., (m % 2) * by, m * bz);
      VVSAdd (mol[n].r, -0.25, region);
      ++ n;
    }
  } else {
    VDiv (gap, region, initUchain);
    for (nz = 0; nz < initUchain.z; nz ++) {
      for (ny = 0; ny < initUchain.y; ny ++) {
        for (nx = 0; nx < initUchain.x; nx ++) {
          VSet (c, nx + 0.25, ny + 0.25, nz + 0.25);
          VMul (c, c, gap);
          VVSAdd (c, -0.5, region);
          for (j = 0; j < 2; j ++) {
            for (m = 0; m < chainLen; m ++) {
              VSet (mol[n].r, 0., (m % 2) * by, m * bz);
              VVSAdd (mol[n].r, 0.5 * j, gap);
              VVAdd (mol[n].r, c);
              ++ n;
            }
          }
        }
      }
    }
  }
  nMol = n;
  ApplyBoundaryCond ();
  VDiv (gap, region, initUcell);
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.5, ny + 0.5, nz + 0.5);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, region);
        for (i = 0; i < nChain * chainLen; i ++) {
          VSub (dr, mol[i].r, c);
          if (VLenSq (dr) < Sqr (rCut)) break;
        }
        if (i == nChain * chainLen) {
          mol[n].r = c;
          ++ n;
        }
      }
    }
  }
  nMol = n;
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


void AssignToChain ()
{
  int i, j, n;

  n = 0;
  for (i = 0; i < nChain; i ++) {
    for (j = 0; j < chainLen; j ++) {
      mol[n].inChain = i;
      ++ n;
    }
  }
  for (n = nChain * chainLen; n < nMol; n ++) mol[n].inChain = -1;
}


void EvalChainProps ()
{
  VecR c, cs, dr, shift;
  real a[3], g[6], gVal[3];
  int i, j, k, n, n1;

  if (countChainProps == 0) {
    bbDistSq = 0.;
    eeDistSq = 0.;
    radGyrSq = 0.;
    gMomRatio1 = 0.;
    gMomRatio2 = 0.;
  }
  n = 0;
  for (i = 0; i < nChain; i ++) {
    VZero (shift);
    VZero (cs);
    for (k = 0; k < 6; k ++) g[k] = 0.;
    n1 = n;
    for (j = 0; j < chainLen; j ++) {
      if (j > 0) {
        VSub (dr, mol[j].r, mol[j - 1].r);
        VShiftWrap (dr, x);
        VShiftWrap (dr, y);
        VShiftWrap (dr, z);
        bbDistSq += VLenSq (dr);
      }
      VAdd (c, mol[n].r, shift);
      VVAdd (cs, c);
      g[0] += Sqr (c.x);
      g[1] += Sqr (c.y);
      g[2] += Sqr (c.z);
      g[3] += c.x * c.y;
      g[4] += c.z * c.x;
      g[5] += c.y * c.z;
      ++ n;
    }
    VVSub (c, mol[n1].r);
    eeDistSq += VLenSq (c);
    VScale (cs, 1. / chainLen);
    for (k = 0; k < 6; k ++) g[k] /= chainLen;
    g[0] -= Sqr (cs.x);
    g[1] -= Sqr (cs.y);
    g[2] -= Sqr (cs.z);
    g[3] -= cs.x * cs.y;
    g[4] -= cs.z * cs.x;
    g[5] -= cs.y * cs.z;
    a[0] = - g[0] - g[1] - g[2];
    a[1] = g[0] * g[1] + g[1] * g[2] + g[2] * g[0] -
       Sqr (g[3]) - Sqr (g[4]) - Sqr (g[5]);
    a[2] = g[0] * Sqr (g[5]) + g[1] * Sqr (g[4]) + g[2] * Sqr (g[3]) -
       2. * g[3] * g[4] * g[5] - g[0] * g[1] * g[2];
    SolveCubic (g, a);
    gVal[0] = Max3 (g[0], g[1], g[2]);
    gVal[2] = Min3 (g[0], g[1], g[2]);
    gVal[1] = g[0] + g[1] + g[2] - gVal[0] - gVal[2];
    radGyrSq += gVal[0] + gVal[1] + gVal[2];
    gMomRatio1 += gVal[1] / gVal[0];
    gMomRatio2 += gVal[2] / gVal[0];
  }
  ++ countChainProps;
  if (countChainProps == limitChainProps) {
    bbDistSq /= nChain * (chainLen - 1) * limitChainProps;
    eeDistSq /= nChain * limitChainProps;
    radGyrSq /= nChain * limitChainProps;
    gMomRatio1 /= nChain * limitChainProps;
    gMomRatio2 /= nChain * limitChainProps;
    PrintChainProps (stdout);
    countChainProps = 0;
  }
}

void PrintChainProps (FILE *fp)
{
  fprintf (fp, "chain props: %.3f %.3f %.3f %.3f %.3f\n",
     sqrt (bbDistSq), eeDistSq, radGyrSq, gMomRatio1, gMomRatio2);
  fflush (fp);
}


void SolveCubic (real *g, real *a)
{
  real q1, q2, t;

  q1 = sqrt (Sqr (a[0]) - 3. * a[1]) / 3.;
  q2 = (a[0] * (2. * Sqr (a[0]) - 9. * a[1]) + 27. * a[2]) / 54.;
  t = acos (q2 / (q1 * q1 * q1));
  g[0] = -2. * q1 * cos (t / 3.) - a[0] / 3.;
  g[1] = -2. * q1 * cos ((t + 2. * M_PI) / 3.) - a[0] / 3.;
  g[2] = -2. * q1 * cos ((t + 4. * M_PI) / 3.) - a[0] / 3.;
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

