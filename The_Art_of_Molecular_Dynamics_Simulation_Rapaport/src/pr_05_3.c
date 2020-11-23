
/* [[pr_05_3 - transport coefficients]] */


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
  VecR r, rv, ra, rf[3];
  real en;
} Mol;
typedef struct {
  VecR *orgVel;
  real *acfVel;
  int count;
  VecR orgTherm, orgVisc;
  real *acfTherm, *acfVisc;
} TBuf;

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
real virSum;
Prop pressure;
real kinEnInitSum;
int stepInitlzTemp;
TBuf *tBuf;
real *avAcfVel, intAcfVel;
int countAcfAv, limitAcfAv, nBuffAcf, nValAcf, stepAcf;
real *avAcfTherm, *avAcfVisc, intAcfTherm, intAcfVisc;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (limitAcfAv),
  NameI (nBuffAcf),
  NameI (nebrTabFac),
  NameI (nValAcf),
  NameR (rNebrShell),
  NameI (stepAcf),
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
  if (stepCount >= stepEquil && (stepCount - stepEquil) %
     stepAcf == 0) EvalVacf ();
}

void SetupJob ()
{
  AllocArrays ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  AccumProps (0);
  InitVacf ();
  kinEnInitSum = 0.;
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
  int k, nb;

  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
  AllocMem (avAcfVel, nValAcf, real);
  AllocMem (avAcfTherm, nValAcf, real);
  AllocMem (avAcfVisc, nValAcf, real);
  AllocMem (tBuf, nBuffAcf, TBuf);
  for (nb = 0; nb < nBuffAcf; nb ++) {
    AllocMem (tBuf[nb].acfVel, nValAcf, real);
    AllocMem (tBuf[nb].orgVel, nMol, VecR);
    AllocMem (tBuf[nb].acfTherm, nValAcf, real);
    AllocMem (tBuf[nb].acfVisc, nValAcf, real);
  }
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
  virSum = 0.;
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
      virSum += fcVal * rr;
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

  DO_MOL VZero (mol[n].ra);
}


void EvalVacf ()
{
  VecR vecTherm, vecVisc;
  int n, nb, ni;

  VZero (vecVisc);
  VZero (vecTherm);
  DO_MOL {
    vecVisc.x += mol[n].rv.y * mol[n].rv.z + 0.5 * mol[n].rf[1].z;
    vecVisc.y += mol[n].rv.z * mol[n].rv.x + 0.5 * mol[n].rf[2].x;
    vecVisc.z += mol[n].rv.x * mol[n].rv.y + 0.5 * mol[n].rf[0].y;
    mol[n].en += VLenSq (mol[n].rv);
    VVSAdd (vecTherm, 0.5 * mol[n].en, mol[n].rv);
    vecTherm.x += 0.5 * VDot(mol[n].rv, mol[n].rf[0]);
    vecTherm.y += 0.5 * VDot(mol[n].rv, mol[n].rf[1]);
    vecTherm.z += 0.5 * VDot(mol[n].rv, mol[n].rf[2]);
  }
  for (nb = 0; nb < nBuffAcf; nb ++) {
    if (tBuf[nb].count == 0) {
      DO_MOL tBuf[nb].orgVel[n] = mol[n].rv;
    }
    if (tBuf[nb].count >= 0) {
      ni = tBuf[nb].count;
      tBuf[nb].acfVel[ni] = 0.;
      DO_MOL tBuf[nb].acfVel[ni] += VDot (tBuf[nb].orgVel[n], mol[n].rv);
    }
    if (tBuf[nb].count == 0) {
      tBuf[nb].orgVisc = vecVisc;
      tBuf[nb].orgTherm = vecTherm;
    }
    tBuf[nb].acfVisc[ni] = VDot (tBuf[nb].orgVisc, vecVisc);
    tBuf[nb].acfTherm[ni] = VDot (tBuf[nb].orgTherm, vecTherm);
    ++ tBuf[nb].count;
  }
  AccumVacf ();
}

void AccumVacf ()
{
  real fac;
  int j, nb;

  for (nb = 0; nb < nBuffAcf; nb ++) {
    if (tBuf[nb].count == nValAcf) {
      for (j = 0; j < nValAcf; j ++) avAcfVel[j] += tBuf[nb].acfVel[j];
      for (j = 0; j < nValAcf; j ++) {
        avAcfVisc[j] += tBuf[nb].acfVisc[j];
        avAcfTherm[j] += tBuf[nb].acfTherm[j];
      }
      tBuf[nb].count = 0;
      ++ countAcfAv;
      if (countAcfAv == limitAcfAv) {
        fac = stepAcf * deltaT / (NDIM * nMol * limitAcfAv);
        intAcfVel = fac * Integrate (avAcfVel, nValAcf);
        for (j = 1; j < nValAcf; j ++) avAcfVel[j] /= avAcfVel[0];
        avAcfVel[0] = 1.;
        fac = density * stepAcf * deltaT /
           (3. * temperature * nMol * limitAcfAv);
        intAcfVisc = fac * Integrate (avAcfVisc, nValAcf);
        for (j = 1; j < nValAcf; j ++) avAcfVisc[j] /= avAcfVisc[0];
        avAcfVisc[0] = 1.;
        fac = density * stepAcf * deltaT /
           (3. * Sqr (temperature) * nMol * limitAcfAv);
        intAcfTherm = fac * Integrate (avAcfTherm, nValAcf);
        for (j = 1; j < nValAcf; j ++) avAcfTherm[j] /= avAcfTherm[0];
        avAcfTherm[0] = 1.;
        PrintVacf (stdout);
        ZeroVacf ();
      }
    }
  }
}

void InitVacf ()
{
  int nb;

  for (nb = 0; nb < nBuffAcf; nb ++)
     tBuf[nb].count = - nb * nValAcf / nBuffAcf;
  ZeroVacf ();
}

void ZeroVacf ()
{
  int j;

  countAcfAv = 0;
  for (j = 0; j < nValAcf; j ++) avAcfVel[j] = 0.;
  for (j = 0; j < nValAcf; j ++) {
    avAcfVisc[j] = 0.;
    avAcfTherm[j] = 0.;
  }
}

void PrintVacf (FILE *fp)
{
  real tVal;
  int j;

  fprintf (fp, "acf\n");
  for (j = 0; j < nValAcf; j ++) {
    tVal = j * stepAcf * deltaT;
    fprintf (fp, "%8.4f %8.4f %8.4f %8.4f\n", tVal,
       avAcfVel[j], avAcfVisc[j], avAcfTherm[j]);
  }
  fprintf (fp, "acf integrals: %8.3f %8.3f %8.3f\n",
     intAcfVel, intAcfVisc, intAcfTherm);
  fflush (fp);
}


real Integrate (real *f, int nf)
{
  real s;
  int i;

  s = 0.5 * (f[0] + f[nf - 1]);
  for (i = 1; i < nf - 1; i ++) s += f[i];
  return (s);
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
  pressure.val = density * (vvSum + virSum) / (nMol * NDIM);
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


void PrintSummary (FILE *fp)
{
  fprintf (fp,
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f\n",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy), PropEst (pressure));
  fflush (fp);
}


#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

