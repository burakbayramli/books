
/* [[pr_10_4 - dihedral angle correlation]] */


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
  int inChain;
} Mol;
typedef struct {
  VecR vec;
  real bLenSq, distSq;
  int site1, site2;
} Cons;

Mol *mol;
Cons *cons;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
real kinEnInitSum;
int stepInitlzTemp;
VecI initUchain;
int chainLen, nChain;
real *consVec, *lMat, bondAng, bondLen, consDevA, consDevL, consPrec;
int *mMat, nChain, nCons, nCycleR, nCycleV, stepRestore;
real *dihedAngCorr, *dihedAngOrg;
int countDihedAngCorr, limitDihedAngCorr, nDihedAng, stepDihedAngCorr;

NameList nameList[] = {
  NameR (bondAng),
  NameR (bondLen),
  NameI (chainLen),
  NameR (consPrec),
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (initUchain),
  NameI (limitDihedAngCorr),
  NameI (nebrTabFac),
  NameR (rNebrShell),
  NameI (stepAvg),
  NameI (stepDihedAngCorr),
  NameI (stepEquil),
  NameI (stepInitlzTemp),
  NameI (stepLimit),
  NameI (stepRestore),
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
  ComputeForces ();
  ComputeChainTorsionForces ();
  ComputeConstraints ();
  CorrectorStep ();
  ApplyBoundaryCond ();
  if (stepCount % stepAvg == 0) AnlzConstraintDevs ();
  nCycleR = nCycleV = 0;
  if (stepCount % stepRestore == 0) {
    RestoreConstraints ();
    ApplyBoundaryCond ();
  }
  EvalProps ();
  if (stepCount < stepEquil) AdjustInitTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (stepCount >= stepEquil && (stepCount - stepEquil) %
     stepDihedAngCorr == 0) EvalDihedAngCorr ();
}

void SetupJob ()
{
  AllocArrays ();
  AssignToChain ();
  BuildConstraintMatrix ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  AccumProps (0);
  RestoreConstraints ();
  kinEnInitSum = 0.;
  nebrNow = 1;
  countDihedAngCorr = 0;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density / 2., 1./3.), initUchain);
  nMol = 0;
  nChain = 2. * VProd (initUchain);
  if (nChain == 2) nChain = 1;
  nMol += nChain * chainLen;
  nCons = 2 * chainLen - 3;
  velMag = sqrt ((NDIM * (1. - 1. / nMol) - (real) nCons / chainLen) *
     temperature);
  VSCopy (cells, 1. / (rCut + rNebrShell), region);
  nebrTabMax = nebrTabFac * nMol;
  nDihedAng = nChain * (chainLen - 3);
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
  AllocMem (cons, nCons, Cons);
  AllocMem (consVec, nCons, real);
  AllocMem (lMat, Sqr (nCons), real);
  AllocMem (mMat, chainLen * nCons, int);
  AllocMem (dihedAngCorr, limitDihedAngCorr, real);
  AllocMem (dihedAngOrg, nDihedAng, real);
}

void PrintSummary (FILE *fp)
{
  fprintf (fp,
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy));
  fprintf (fp, "\n");
  fprintf (fp, "constraint devs: %.3e %.3e cycles: %d %d\n",
     consDevL, consDevA, nCycleR, nCycleV);
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
              if ((m1 != m2 || j2 < j1) && (mol[j1].inChain == -1 ||
                 mol[j1].inChain != mol[j2].inChain ||
                 abs (j1 - j2) > 3)) {
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


void ComputeChainTorsionForces ()
{
  VecR dr1, dr2, dr3, w1, w2;
  real c, c11, c12, c13, c22, c23, c33, ca, cb1, cb2, cd,
     cr1, cr2, f, t1, t2, t3, t4, t5, t6,
     g[6] = {1.000, 1.310, -1.414, -0.330, 2.828, -3.394},
     tCon = 15.50;
  int i, n, nn;

  for (n = 0; n < nChain; n ++) {
    for (i = 0; i < chainLen - 3; i ++) {
      nn = n * chainLen + i;
      VSub (dr1, mol[nn + 1].r, mol[nn].r);
      VWrapAll (dr1);
      VSub (dr2, mol[nn + 2].r, mol[nn + 1].r);
      VWrapAll (dr2);
      VSub (dr3, mol[nn + 3].r, mol[nn + 2].r);
      VWrapAll (dr3);
      c11 = VLenSq (dr1);
      c12 = VDot (dr1, dr2);
      c13 = VDot (dr1, dr3);
      c22 = VLenSq (dr2);
      c23 = VDot (dr2, dr3);
      c33 = VLenSq (dr3);
      ca = c13 * c22 - c12 * c23;
      cb1 = c11 * c22 - c12 * c12;
      cb2 = c22 * c33 - c23 * c23;
      cd = sqrt (cb1 * cb2);
      c = ca / cd;
      f = - tCon * (g[1] + (2. * g[2] + (3. * g[3] + (4. * g[4] +
         5. * g[5] * c) * c) * c) * c);
      t1 = ca;
      t2 = c11 * c23 - c12 * c13;
      t3 = - cb1;
      t4 = cb2;
      t5 = c13 * c23 - c12 * c33;
      t6 = - ca;
      cr1 = c12 / c22;
      cr2 = c23 / c22;
      VSSAdd (w1, t1, dr1, t2, dr2);
      VVSAdd (w1, t3, dr3);
      VScale (w1, f * c22 / (cd * cb1));
      VSSAdd (w2, t4, dr1, t5, dr2);
      VVSAdd (w2, t6, dr3);
      VScale (w2, f * c22 / (cd * cb2));
      VVAdd (mol[nn].ra, w1);
      VVSAdd (mol[nn + 1].ra, - (1. + cr1), w1);
      VVSAdd (mol[nn + 1].ra, cr2, w2);
      VVSAdd (mol[nn + 2].ra, cr1, w1);
      VVSAdd (mol[nn + 2].ra, - (1. + cr2), w2);
      VVAdd (mol[nn + 3].ra, w2);
      uSum += tCon * (g[0] + (g[1] + (g[2] + (g[3] + (g[4] + g[5] * c) *
         c) * c) * c) * c);
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
  real by, bz;
  int j, m, n, nx, ny, nz;

  by = bondLen * cos (bondAng / 2.);
  bz = bondLen * sin (bondAng / 2.);
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
  ApplyBoundaryCond ();
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


void BuildConstraintMatrix ()
{
  int i, m;

  for (i = 0; i < chainLen * nCons; i ++) mMat[i] = 0;
  for (i = 0; i < chainLen; i ++) {
    m = 2 * i - 3;
    if (m >= 0) mMat[m * chainLen + i] = 2;
    ++ m;
    if (m >= 0) mMat[m * chainLen + i] = 2;
    m += 2;
    if (m < nCons) mMat[m * chainLen + i] = -2;
    ++ m;
    if (m < nCons) mMat[m * chainLen + i] = -2;
  }
  for (m = 0; m < nCons; m ++) {
    cons[m].distSq = Sqr (bondLen);
    if (m % 2 == 1) cons[m].distSq *= 2. * (1. - cos (bondAng));
    cons[m].site1 = m / 2;
    cons[m].site2 = (m + 3) / 2;
  }
}


void ComputeConstraints ()
{
  VecR da, dv;
  real w;
  int i, m, m1, m2, mDif, n, nn;

  for (n = 0; n < nChain; n ++) {
    nn = n * chainLen;
    for (m = 0; m < nCons; m ++) {
      VSub (cons[m].vec, mol[nn + cons[m].site1].r,
         mol[nn + cons[m].site2].r);
      VWrapAll (cons[m].vec);
    }
    m = 0;
    for (m1 = 0; m1 < nCons; m1 ++) {
      for (m2 = 0; m2 < nCons; m2 ++) {
        lMat[m] = 0.;
        mDif = mMat[m1 * chainLen + cons[m2].site1] -
           mMat[m1 * chainLen + cons[m2].site2];
        if (mDif != 0) lMat[m] = mDif * VDot (cons[m1].vec, cons[m2].vec);
        ++ m;
      }
    }
    for (m = 0; m < nCons; m ++) {
      VSub (dv, mol[nn + cons[m].site1].rv, mol[nn + cons[m].site2].rv);
      VSub (da, mol[nn + cons[m].site1].ra, mol[nn + cons[m].site2].ra);
      consVec[m] = - VDot (da, cons[m].vec) - VLenSq (dv);
    }
    SolveLineq (lMat, consVec, nCons);
    for (m = 0; m < nCons; m ++) {
      for (i = 0; i < chainLen; i ++) {
        w = mMat[m * chainLen + i];
        if (w != 0.) VVSAdd (mol[nn + i].ra, w * consVec[m], cons[m].vec);
      }
    }
  }
}

void RestoreConstraints ()
{
  VecR dr, dv;
  real cDev, cDevR, cDevV, g, ga;
  int changed, m, m1, m2, maxCycle, n;

  maxCycle = 200;
  cDevR = cDevV = 0.;
  for (n = 0; n < nChain; n ++) {
    nCycleR = 0;
    changed = 1;
    while (nCycleR < maxCycle && changed) {
      ++ nCycleR;
      changed = 0;
      cDev = 0.;
      for (m = 0; m < nCons; m ++) {
        m1 = n * chainLen + cons[m].site1;
        m2 = n * chainLen + cons[m].site2;
        VSub (dr, mol[m1].r, mol[m2].r);
        VWrapAll (dr);
        g = (VLenSq (dr) - cons[m].distSq) / (4. * cons[m].distSq);
        ga = fabs (g);
        cDev = Max (cDev, ga);
        if (ga > consPrec) {
          changed = 1;
          VVSAdd (mol[m1].r, - g, dr);
          VVSAdd (mol[m2].r, g, dr);
        }
      }
    }
    cDevR = Max (cDevR, cDev);
    nCycleV = 0;
    changed = 1;
    while (nCycleV < maxCycle && changed) {
      ++ nCycleV;
      changed = 0;
      cDev = 0.;
      for (m = 0; m < nCons; m ++) {
        m1 = n * chainLen + cons[m].site1;
        m2 = n * chainLen + cons[m].site2;
        VSub (dr, mol[m1].r, mol[m2].r);
        VWrapAll (dr);
        VSub (dv, mol[m1].rv, mol[m2].rv);
        g = VDot (dv, dr) / (2. * cons[m].distSq);
        ga = fabs (g);
        cDev = Max (cDev, ga);
        if (ga > consPrec) {
          changed = 1;
          VVSAdd (mol[m1].rv, - g, dr);
          VVSAdd (mol[m2].rv, g, dr);
        }
      }
    }
    cDevV = Max (cDevV, cDev);
  }
}


void AnlzConstraintDevs ()
{
  VecR dr1;
  real sumL;
  int i, n, ni;
  VecR dr2;
  real sumA;

  sumL = 0.;
  sumA = 0.;
  for (n = 0; n < nChain; n ++) {
    for (i = 0; i < chainLen - 1; i ++) {
      ni = n * chainLen + i;
      VSub (dr1, mol[ni + 1].r, mol[ni].r);
      VWrapAll (dr1);
      cons[i].bLenSq = VLenSq (dr1);
      sumL += cons[i].bLenSq;
    }
    for (i = 1; i < chainLen - 1; i ++) {
      ni = n * chainLen + i;
      VSub (dr1, mol[ni + 1].r, mol[ni].r);
      VWrapAll (dr1);
      VSub (dr2, mol[ni - 1].r, mol[ni].r);
      VWrapAll (dr2);
      sumA += Sqr (VDot (dr1, dr2)) / (cons[i - 1].bLenSq * cons[i].bLenSq);
    }
  }
  consDevL = sqrt (sumL / (nChain * (chainLen - 1))) - bondLen;
  consDevA = sqrt (sumA / (nChain * (chainLen - 2))) - cos (M_PI - bondAng);
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


void EvalDihedAngCorr ()
{
  VecR dr1, dr2, dr3, w;
  real c11, c12, c13, c22, cosAngSq, dihedAng, t;
  int i, j, n, nn;

  dihedAngCorr[countDihedAngCorr] = 0.;
  j = 0;
  for (n = 0; n < nChain; n ++) {
    for (i = 0; i < chainLen - 3; i ++) {
      nn = n * chainLen + i;
      VSub (dr1, mol[nn + 1].r, mol[nn].r);
      VWrapAll (dr1);
      VSub (dr2, mol[nn + 2].r, mol[nn + 1].r);
      VWrapAll (dr2);
      VSub (dr3, mol[nn + 3].r, mol[nn + 2].r);
      VWrapAll (dr3);
      c11 = VLenSq (dr1);
      c12 = VDot (dr1, dr2);
      c13 = VDot (dr1, dr3);
      c22 = VLenSq (dr2);
      cosAngSq = Sqr (c12) / (c11 * c22);
      t = (c13 / Sqr (bondLen) - cosAngSq) / (1. - cosAngSq);
      if (fabs (t) > 1.) t = Sgn (1., t);
      dihedAng = acos (t);
      VCross (w, dr2, dr3);
      if (VDot (dr1, w) < 0.) dihedAng = 2. * M_PI - dihedAng;
      if (countDihedAngCorr == 0) dihedAngOrg[j] = dihedAng;
      dihedAngCorr[countDihedAngCorr] +=
         cos (dihedAng - dihedAngOrg[j]);
      ++ j;
    }
  }
  ++ countDihedAngCorr;
  if (countDihedAngCorr == limitDihedAngCorr) {
    for (n = 0; n < limitDihedAngCorr; n ++) dihedAngCorr[n] /= nDihedAng;
    PrintDihedAngCorr (stdout);
    countDihedAngCorr = 0;
  }
}

void PrintDihedAngCorr (FILE *fp)
{
  real tVal;
  int n;

  for (n = 0; n < limitDihedAngCorr; n ++) {
    tVal = n * stepDihedAngCorr * deltaT;
    fprintf (fp, "%8.4f %8.4f\n", tVal, dihedAngCorr[n]);
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


#define MAT(a, n, i, j)  (a)[(i) + n * (j)]
#define A(i, j)  MAT (a, n, i, j)

#define Swap(a, b)  v = a, a = b, b = v

#define N_MAX  100

void SolveLineq (real *a, real *x, int n)
{
  real vMaxI[N_MAX], v, vMax;
  int ptrMax[N_MAX], i, j, k, m;

  if (n > N_MAX) exit (0);
  for (i = 0; i < n; i ++) {
    vMax = 0.;
    for (j = 0; j < n; j ++) {
      if ((v = fabs (A(i, j))) > vMax) vMax = v;
    }
    vMaxI[i] = 1. / vMax;
  }
  for (m = 0; m < n; m ++) {
    vMax = 0.;
    for (i = m; i < n; i ++) {
      for (k = 0; k < m; k ++) A(i, m) -= A(i, k) * A(k, m);
      if ((v = fabs (A(i, m)) * vMaxI[i]) > vMax) {
        vMax = v;
        ptrMax[m] = i;
      }
    }
    if (m != ptrMax[m]) {
      for (k = 0; k < n ; k ++)
         Swap (A(m, k), A(ptrMax[m], k));
      vMaxI[ptrMax[m]] = vMaxI[m];
    }
    for (j = m + 1; j < n; j ++) {
      for (k = 0; k < m; k ++) A(m, j) -= A(m, k) * A(k, j);
      A(m, j) /= A(m, m);
    }
  }
  for (i = 0; i < n; i ++) {
    Swap (x[ptrMax[i]], x[i]);
    for (j = 0; j < i; j ++) x[i] -= A(i, j) * x[j];
    x[i] /= A(i, i);
  }
  for (i = n - 2; i >= 0; i --) {
    for (j = i + 1; j < n; j ++) x[i] -= A(i, j) * x[j];
  }
}


#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

