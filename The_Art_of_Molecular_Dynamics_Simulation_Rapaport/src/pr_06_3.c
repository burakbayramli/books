
/* [[pr_06_3 - constrained PT]] */


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
  VecR r, rv, ra, ra1, ra2, ro, rvo;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
VecI cells;
int *cellList;
real virSum;
Prop pressure;
real dilateRate, dilateRate1, dilateRate2, dvirSum1, dvirSum2,
   extPressure, tolPressure, varL, varLo, varLv, varLv1, varLv2;
int maxEdgeCells, nPressCycle, stepAdjustPress;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameR (extPressure),
  NameI (initUcell),
  NameI (stepAdjustPress),
  NameI (stepAvg),
  NameI (stepEquil),
  NameI (stepLimit),
  NameR (temperature),
  NameR (tolPressure),
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
  PredictorStep ();
  PredictorStepBox ();
  ApplyBoundaryCond ();
  UpdateCellSize ();
  UnscaleCoords ();
  ComputeForces ();
  ApplyBarostat ();
  ApplyThermostat ();
  CorrectorStep ();
  CorrectorStepBox ();
  ApplyBoundaryCond ();
  EvalProps ();
  nPressCycle = 0;
  if (stepCount % stepAdjustPress == 0) AdjustPressure ();
  if (stepCount % stepAdjustPress == 10) AdjustTemp ();
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
  InitBoxVars ();
  ScaleCoords ();
  ScaleVels ();
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density / 4., 1./3.), initUcell);
  nMol = 4 * VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / rCut, region);
  maxEdgeCells = 1.3 * cells.x;
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, Cube (maxEdgeCells) + nMol, int);
}

void PrintSummary (FILE *fp)
{
  fprintf (fp,
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy));
  fprintf (fp, " %7.4f %7.4f", PropEst (pressure));
  fprintf (fp, " %7.4f", region.x);
  fprintf (fp, "\n");
  fflush (fp);
}

void ComputeForces ()
{
  VecR dr, invWid, rs, shift;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real fcVal, rr, rrCut, rri, rri3, uVal;
  VecR dv;
  real w;
  int c, j1, j2, m1, m1x, m1y, m1z, m2, n, offset;

  rrCut = Sqr (rCut);
  VDiv (invWid, cells, region);
  for (n = nMol; n < nMol + VProd (cells); n ++) cellList[n] = -1;
  DO_MOL {
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nMol;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  virSum = 0.;
  dvirSum1 = dvirSum2 = 0.;
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
                rr = VLenSq (dr);
                if (rr < rrCut) {
                  rri = 1. / rr;
                  rri3 = Cube (rri);
                  fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
                  uVal = 4. * rri3 * (rri3 - 1.) + 1.;
                  VVSAdd (mol[j1].ra, fcVal, dr);
                  VVSAdd (mol[j2].ra, - fcVal, dr);
                  uSum += uVal;
                  virSum += fcVal * rr;
                  w = 144. * rri3 * (4. * rri3 - 1.) * rri;
                  VSub (dv, mol[j1].rv, mol[j2].rv);
                  dvirSum1 += w * VDot (dr, dv);
                  dvirSum2 += w * rr;
                }
              }
            }
          }
        }
      }
    }
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


void PredictorStepBox ()
{
  real c[] = {23.,-16.,5.}, div = 12.;

  varLv = dilateRate * varL;
  varLo = varL;
  varL = varL + (deltaT / div) * (c[0] * varLv + c[1] * varLv1 +
     c[2] * varLv2);
  varLv2 = varLv1;
  varLv1 = varLv;
  dilateRate2 = dilateRate1;
  dilateRate1 = dilateRate;
  VSetAll (region, varL);
}

void CorrectorStepBox ()
{
  real c[] = {5.,8.,-1.}, div = 12.;

  varLv = dilateRate * varL;
  varL = varLo + (deltaT / div) * (c[0] * varLv + c[1] * varLv1 +
     c[2] * varLv2);
  VSetAll (region, varL);
}


#undef VWrap

#define VWrap(v, t)                                         \
   if (v.t >= 0.5) v.t -= 1.;                               \
   else if (v.t < -0.5) v.t += 1.

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
  vvSum *= Sqr (varL);
  vFac = velMag / sqrt (vvSum / nMol);
  DO_MOL VScale (mol[n].rv, vFac);
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
  DO_MOL
     VSSAdd (mol[n].ra, 1. / varL, mol[n].ra,
        vFac / varL - dilateRate, mol[n].rv);
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


void ApplyBarostat ()
{
  real vvS;
  int n;

  vvS = 0.;
  DO_MOL vvS += VLenSq (mol[n].rv);
  dilateRate = - dvirSum1 * varL / (3. * (vvS * Sqr (varL) +
     virSum) + dvirSum2);
}

void AdjustPressure ()
{
  real rFac, w;
  int maxPressCycle, n;

  maxPressCycle = 20;
  if (fabs (pressure.val - extPressure) > tolPressure * extPressure) {
    UnscaleCoords ();
    vvSum = vvSum / Sqr (varL);
    for (nPressCycle = 0; nPressCycle < maxPressCycle; nPressCycle ++) {
      UpdateCellSize ();
      ComputeForces ();
      w = 3. * Cube (varL);
      pressure.val = (vvSum * Sqr (varL) + virSum) / w;
      rFac = 1. + (pressure.val - extPressure) /
         (3. * pressure.val + dvirSum2 / w);
      DO_MOL VScale (mol[n].r, rFac);
      VScale (region, rFac);
      varL *= rFac;
      if (fabs (pressure.val - extPressure) <
         tolPressure * extPressure) break;
    }
    ScaleCoords ();
    vvSum *= Sqr (varL);
  }
}

void ScaleCoords ()
{
  real fac;
  int n;

  fac = 1. / varL;
  DO_MOL VScale (mol[n].r, fac);
}

void UnscaleCoords ()
{
  real fac;
  int  n;

  fac = varL;
  DO_MOL VScale (mol[n].r, fac);
}

void ScaleVels ()
{
  real fac;
  int n;

  fac = 1. / varL;
  DO_MOL VScale (mol[n].rv, fac);
}

void UpdateCellSize ()
{
  VSCopy (cells, 1. / rCut, region);
  cells.x = Min (cells.x, maxEdgeCells);
  cells.y = Min (cells.y, maxEdgeCells);
  cells.z = Min (cells.z, maxEdgeCells);
}

void InitBoxVars ()
{
  varL = region.x;
  varLv = 0.;
  varLv1 = 0.;
  varLv2 = 0.;
  dilateRate1 = 0.;
  dilateRate2 = 0.;
}

void EvalProps ()
{
  real vv;
  int n;

  VZero (vSum);
  vvSum = 0.;
  DO_MOL {
    VVAdd (vSum, mol[n].rv);
    vv = VLenSq (mol[n].rv);
    vvSum += vv;
  }
  vvSum *= Sqr (varL);
  kinEnergy.val = 0.5 * vvSum / nMol;
  totEnergy.val = kinEnergy.val + uSum / nMol;
  pressure.val = (vvSum + virSum) / (3. * Cube (varL));
}


void AccumProps (int icode)
{
  if (icode == 0) {
    PropZero (totEnergy);
    PropZero (kinEnergy);
    PropZero (pressure);
  } else if (icode == 1) {
    PropAccum (totEnergy);
    PropAccum (kinEnergy);
    PropAccum (pressure);
  } else if (icode == 2) {
    PropAvg (totEnergy, stepAvg);
    PropAvg (kinEnergy, stepAvg);
    PropAvg (pressure, stepAvg);
  }
}


#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

