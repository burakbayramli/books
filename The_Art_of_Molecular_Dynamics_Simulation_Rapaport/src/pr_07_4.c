
/* [[pr_07_4 - homogeneous heat flow]] */


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
  VecR r, rv, ra, ra1, ra2, ro, rvo, rf[3];
  real en;
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
real heatForce;
Prop thermalCond;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameR (heatForce),
  NameI (initUcell),
  NameI (nebrTabFac),
  NameR (rNebrShell),
  NameI (stepAvg),
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
  if (nebrNow) {
    nebrNow = 0;
    dispHi = 0.;
    BuildNebrList ();
  }
  PredictorStep ();
  ApplyBoundaryCond ();
  ComputeForces ();
  ComputeThermalForce ();
  ApplyThermostat ();
  CorrectorStep ();
  ApplyBoundaryCond ();
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
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density / 4., 1./3.), initUcell);
  nMol = 4 * VProd (initUcell);
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

void PrintSummary (FILE *fp)
{
  fprintf (fp,
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy));
  fprintf (fp, " %7.4f %7.4f", PropEst (thermalCond));
  fprintf (fp, "\n");
  fflush (fp);
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
  VecR w[3], dr;
  real fcVal, rr, rrCut, rri, rri3, uVal;
  int j1, j2, n;
  int k;

  rrCut = Sqr (rCut);
  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  DO_MOL {
    mol[n].en = 0.;
    for (k = 0; k < 3; k ++) VZero (mol[n].rf[k]);
  }
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
      mol[j1].en += uVal;
      mol[j2].en += uVal;
      for (k = 0; k < 3; k ++) w[k] = dr;
      VScale (w[0], fcVal * dr.x);
      VScale (w[1], fcVal * dr.y);
      VScale (w[2], fcVal * dr.z);
      for (k = 0; k < 3; k ++) {
        VVAdd (mol[j1].rf[k], w[k]);
        VVAdd (mol[j2].rf[k], w[k]);
      }
    }
  }
}


void ComputeThermalForce ()
{
  VecR rfMolSumZ;
  real enMolSum;
  int n;

  VZero (rfMolSumZ);
  enMolSum = 0.;
  DO_MOL {
    VVAdd (rfMolSumZ, mol[n].rf[2]);
    mol[n].en += VLenSq (mol[n].rv);
    enMolSum += mol[n].en;
  }
  DO_MOL {
    VVSAdd (mol[n].rf[2], -1. / nMol, rfMolSumZ);
    mol[n].en = 0.5 * (mol[n].en - enMolSum / nMol);
  }
  DO_MOL {
    mol[n].ra.z += mol[n].en * heatForce;
    VVSAdd (mol[n].ra, 0.5 * heatForce, mol[n].rf[2]);
  }
}


#define PCR4(r, ro, v, a, a1, a2, t)                        \
   r.t = ro.t + deltaT * v.t +                              \
   wr * (cr[0] * a.t + cr[1] * a1.t + cr[2] * a2.t)
#define PCV4(r, ro, v, a, a1, a2, t)                        \
   v.t = (r.t - ro.t) / deltaT +                            \
   wv * (cv[0] * a.t + cv[1] * a1.t + cv[2] * a2.t)

#define PR(t)                                               \
   PCR4 (mol[n].r, mol[n].r, mol[n].rv,                     \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)
#define PRV(t)                                              \
   PCV4 (mol[n].r, mol[n].ro, mol[n].rv,                    \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)
#define CR(t)                                               \
   PCR4 (mol[n].r, mol[n].ro, mol[n].rvo,                   \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)
#define CRV(t)                                              \
   PCV4 (mol[n].r, mol[n].ro, mol[n].rv,                    \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)

void PredictorStep ()
{
  real cr[] = {19.,-10.,3.}, cv[] = {27.,-22.,7.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    mol[n].ro = mol[n].r;
    mol[n].rvo = mol[n].rv;
    PR (x);
    PRV (x);
    PR (y);
    PRV (y);
    PR (z);
    PRV (z);
    mol[n].ra2 = mol[n].ra1;
    mol[n].ra1 = mol[n].ra;
  }
}

void CorrectorStep ()
{
  real cr[] = {3.,10.,-1.}, cv[] = {7.,6.,-1.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    CR (x);
    CRV (x);
    CR (y);
    CRV (y);
    CR (z);
    CRV (z);
  }
}


void ApplyThermostat ()
{
  real s1, s2, vFac;
  int n;

  s1 = s2 = 0.;
  DO_MOL {
    s1 += VDot (mol[n].rv, mol[n].ra);
    s2 += VLenSq (mol[n].rv);
  }
  vFac = - s1 / s2;
  DO_MOL VVSAdd (mol[n].ra, vFac, mol[n].rv);
}


void ApplyBoundaryCond ()
{
  int n;

  DO_MOL VWrapAll (mol[n].r);
}


void InitCoords ()
{
  VecR c, gap;
  int j, n, nx, ny, nz;

  VDiv (gap, region, initUcell);
  n = 0;
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.25, ny + 0.25, nz + 0.25);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, region);
        for (j = 0; j < 4; j ++) {
          mol[n].r = c;
          if (j != 3) {
            if (j != 0) mol[n].r.x += 0.5 * gap.x;
            if (j != 1) mol[n].r.y += 0.5 * gap.y;
            if (j != 2) mol[n].r.z += 0.5 * gap.z;
          }
          ++ n;
        }
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

  DO_MOL {
    VZero (mol[n].ra);
    VZero (mol[n].ra1);
    VZero (mol[n].ra2);
  }
}


void EvalProps ()
{
  real vv, vvMax;
  int n;
  real thermVecZ;

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
  thermVecZ = 0.;
  DO_MOL thermVecZ += mol[n].rv.z * mol[n].en +
     0.5 * VDot (mol[n].rf[2], mol[n].rv);
  thermalCond.val = thermVecZ * density / (temperature *
     heatForce * nMol);
}

void AccumProps (int icode)
{
  if (icode == 0) {
    PropZero (totEnergy);
    PropZero (kinEnergy);
    PropZero (thermalCond);
  } else if (icode == 1) {
    PropAccum (totEnergy);
    PropAccum (kinEnergy);
    PropAccum (thermalCond);
  } else if (icode == 2) {
    PropAvg (totEnergy, stepAvg);
    PropAvg (kinEnergy, stepAvg);
    PropAvg (thermalCond, stepAvg);
  }
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

