
/* [[pr_13_3 - fast-multipole method]] */


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
  real chg;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, potEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
real **histRdf, **cumRdf, rangeRdf;
int countRdf, limitRdf, sizeHistRdf, stepRdf;
real kinEnInitSum;
int stepInitlzTemp;
MpCell **mpCell;
VecR cellWid;
VecI mpCells;
real chargeMag;
int *mpCellList, curCellsEdge, curLevel, maxCellsEdge, maxLevel, maxOrd,
   wellSep;

NameList nameList[] = {
  NameR (chargeMag),
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (limitRdf),
  NameI (maxLevel),
  NameI (nebrTabFac),
  NameR (rangeRdf),
  NameR (rNebrShell),
  NameI (sizeHistRdf),
  NameI (stepAvg),
  NameI (stepEquil),
  NameI (stepInitlzTemp),
  NameI (stepLimit),
  NameI (stepRdf),
  NameR (temperature),
  NameI (wellSep),
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
  if (nebrNow) {
    nebrNow = 0;
    dispHi = 0.;
    BuildNebrList ();
  }
  ComputeForces ();
  MultipoleCalc ();
  ComputeWallForces ();
  ApplyThermostat ();
  LeapfrogStep (2);
  EvalProps ();
  if (stepCount < stepEquil) AdjustInitTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (stepCount >= stepEquil && (stepCount - stepEquil) % stepRdf == 0)
     EvalRdf ();
}

void SetupJob ()
{
  AllocArrays ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  InitCharges ();
  AccumProps (0);
  nebrNow = 1;
  kinEnInitSum = 0.;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nMol = VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / (rCut + rNebrShell), region);
  nebrTabMax = nebrTabFac * nMol;
  maxOrd = MAX_MPEX_ORD;
}

void AllocArrays ()
{
  int k, n;

  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
  AllocMem (mpCell, maxLevel + 1, MpCell *);
  maxCellsEdge = 2;
  for (n = 2; n <= maxLevel; n ++) {
    maxCellsEdge *= 2;
    VSetAll (mpCells, maxCellsEdge);
    AllocMem (mpCell[n], VProd (mpCells), MpCell);
  }
  AllocMem (mpCellList, nMol + VProd (mpCells), int);
  AllocMem2 (histRdf, 2, sizeHistRdf, real);
  AllocMem2 (cumRdf, 2, sizeHistRdf, real);
}


void BuildNebrList ()
{
  VecR dr, invWid, rs;
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
          if (m2v.x < 0 || m2v.x >= cells.x ||
              m2v.y < 0 || m2v.y >= cells.y ||
                           m2v.z >= cells.z) continue;
          m2 = VLinear (m2v, cells) + nMol;
          DO_CELL (j1, m1) {
            DO_CELL (j2, m2) {
              if (m1 != m2 || j2 < j1) {
                VSub (dr, mol[j1].r, mol[j2].r);
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


void MultipoleCalc ()
{
  int j, k, m1;

  VSetAll (mpCells, maxCellsEdge);
  AssignMpCells ();
  VDiv (cellWid, region, mpCells);
  EvalMpCell ();
  curCellsEdge = maxCellsEdge;
  for (curLevel = maxLevel - 1; curLevel >= 2; curLevel --) {
    curCellsEdge /= 2;
    VSetAll (mpCells, curCellsEdge);
    VDiv (cellWid, region, mpCells);
    CombineMpCell ();
  }
  for (m1 = 0; m1 < 64; m1 ++) {
    for (j = 0; j <= maxOrd; j ++) {
      for (k = 0; k <= j; k ++) {
        mpCell[2][m1].me.c(j, k) = 0.;
        mpCell[2][m1].me.s(j, k) = 0.;
      }
    }
  }
  curCellsEdge = 2;
  for (curLevel = 2; curLevel <= maxLevel; curLevel ++) {
    curCellsEdge *= 2;
    VSetAll (mpCells, curCellsEdge);
    VDiv (cellWid, region, mpCells);
    GatherWellSepLo ();
    if (curLevel < maxLevel) PropagateCellLo ();
  }
  ComputeFarCellInt ();
  ComputeNearCellInt ();
}


void AssignMpCells ()
{
  VecR invWid, rs;
  VecI cc;
  int c, n;

  VDiv (invWid, mpCells, region);
  for (n = nMol; n < nMol + VProd (mpCells); n ++) mpCellList[n] = -1;
  DO_MOL {
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, mpCells) + nMol;
    mpCellList[n] = mpCellList[c];
    mpCellList[c] = n;
  }
}

#define DO_MP_CELL(j, m)                                    \
   for (j = mpCellList[m + nMol]; j >= 0; j = mpCellList[j])

void EvalMpCell ()
{
  MpTerms le;
  VecR cMid, dr;
  VecI m1v;
  int j, j1, k, m1, m1x, m1y, m1z;

  for (m1z = 0; m1z < mpCells.z; m1z ++) {
    for (m1y = 0; m1y < mpCells.y; m1y ++) {
      for (m1x = 0; m1x < mpCells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, mpCells);
        mpCell[maxLevel][m1].occ = 0;
        for (j = 0; j <= maxOrd; j ++) {
          for (k = 0; k <= j; k ++) {
            mpCell[maxLevel][m1].le.c(j, k) = 0.;
            mpCell[maxLevel][m1].le.s(j, k) = 0.;
          }
        }
        if (mpCellList[m1 + nMol] >= 0) {
          VAddCon (cMid, m1v, 0.5);
          VMul (cMid, cMid, cellWid);
          VVSAdd (cMid, - 0.5, region);
          DO_MP_CELL (j1, m1) {
            ++ mpCell[maxLevel][m1].occ;
            VSub (dr, mol[j1].r, cMid);
            EvalMpL (&le, &dr, maxOrd);
            for (j = 0; j <= maxOrd; j ++) {
              for (k = 0; k <= j; k ++) {
                mpCell[maxLevel][m1].le.c(j, k) += mol[j1].chg * le.c(j, k);
                mpCell[maxLevel][m1].le.s(j, k) += mol[j1].chg * le.s(j, k);
              }
            }
          }
        }
      }
    }
  }
}

void CombineMpCell ()
{
  MpTerms le, le2;
  VecR rShift;
  VecI m1v, m2v, mpCellsN;
  int iDir, j, k, m1, m1x, m1y, m1z, m2;

  VSCopy (mpCellsN, 2, mpCells);
  for (m1z = 0; m1z < mpCells.z; m1z ++) {
    for (m1y = 0; m1y < mpCells.y; m1y ++) {
      for (m1x = 0; m1x < mpCells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, mpCells);
        for (j = 0; j <= maxOrd; j ++) {
          for (k = 0; k <= j; k ++) {
            mpCell[curLevel][m1].le.c(j, k) = 0.;
            mpCell[curLevel][m1].le.s(j, k) = 0.;
          }
        }
        mpCell[curLevel][m1].occ = 0;
        for (iDir = 0; iDir < 8; iDir ++) {
          VSCopy (m2v, 2, m1v);
          VSCopy (rShift, -0.25, cellWid);
          if (IsOdd (iDir)) {
            ++ m2v.x;
            rShift.x *= -1.;
          }
          if (IsOdd (iDir / 2)) {
            ++ m2v.y;
            rShift.y *= -1.;
          }
          if (IsOdd (iDir / 4)) {
            ++ m2v.z;
            rShift.z *= -1.;
          }
          m2 = VLinear (m2v, mpCellsN);
          if (mpCell[curLevel + 1][m2].occ == 0) continue;
          mpCell[curLevel][m1].occ += mpCell[curLevel + 1][m2].occ;
          EvalMpL (&le2, &rShift, maxOrd);
          EvalMpProdLL (&le, &mpCell[curLevel + 1][m2].le, &le2, maxOrd);
          for (j = 0; j <= maxOrd; j ++) {
            for (k = 0; k <= j; k ++) {
              mpCell[curLevel][m1].le.c(j, k) += le.c(j, k);
              mpCell[curLevel][m1].le.s(j, k) += le.s(j, k);
            }
          }
        }
      }
    }
  }
}

#define LoLim(t)  IsEven (m1v.t) - 2 * wellSep
#define HiLim(t)  IsEven (m1v.t) + 2 * wellSep + 1

void GatherWellSepLo ()
{
  MpTerms le, me, me2;
  VecR rShift;
  VecI m1v, m2v;
  real s;
  int j, k, m1, m1x, m1y, m1z, m2, m2x, m2y, m2z;

  for (m1z = 0; m1z < mpCells.z; m1z ++) {
    for (m1y = 0; m1y < mpCells.y; m1y ++) {
      for (m1x = 0; m1x < mpCells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, mpCells);
        if (mpCell[curLevel][m1].occ == 0) continue;
        for (m2z = LoLim (z); m2z <= HiLim (z); m2z ++) {
          for (m2y = LoLim (y); m2y <= HiLim (y); m2y ++) {
            for (m2x = LoLim (x); m2x <= HiLim (x); m2x ++) {
              VSet (m2v, m2x, m2y, m2z);
              if (m2v.x < 0 || m2v.x >= mpCells.x ||
                  m2v.y < 0 || m2v.y >= mpCells.y ||
                  m2v.z < 0 || m2v.z >= mpCells.z) continue;
              if (abs (m2v.x - m1v.x) <= wellSep &&
                  abs (m2v.y - m1v.y) <= wellSep &&
                  abs (m2v.z - m1v.z) <= wellSep) continue;
              m2 = VLinear (m2v, mpCells);
              if (mpCell[curLevel][m2].occ == 0) continue;
              for (j = 0; j <= maxOrd; j ++) {
                s = (IsOdd (j)) ? -1. : 1.;
                for (k = 0; k <= j; k ++) {
                  le.c(j, k) = s * mpCell[curLevel][m2].le.c(j, k);
                  le.s(j, k) = s * mpCell[curLevel][m2].le.s(j, k);
                }
              }
              VSub (rShift, m2v, m1v);
              VMul (rShift, rShift, cellWid);
              EvalMpM (&me2, &rShift, maxOrd);
              EvalMpProdLM (&me, &le, &me2, maxOrd);
              for (j = 0; j <= maxOrd; j ++) {
                for (k = 0; k <= j; k ++) {
                  mpCell[curLevel][m1].me.c(j, k) += me.c(j, k);
                  mpCell[curLevel][m1].me.s(j, k) += me.s(j, k);
                }
              }
            }
          }
        }
      }
    }
  }
}

void PropagateCellLo ()
{
  MpTerms le;
  VecR rShift;
  VecI m1v, m2v, mpCellsN;
  int iDir, m1, m1x, m1y, m1z, m2;

  VSCopy (mpCellsN, 2, mpCells);
  for (m1z = 0; m1z < mpCells.z; m1z ++) {
    for (m1y = 0; m1y < mpCells.y; m1y ++) {
      for (m1x = 0; m1x < mpCells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, mpCells);
        if (mpCell[curLevel][m1].occ == 0) continue;
        for (iDir = 0; iDir < 8; iDir ++) {
          VSCopy (m2v, 2, m1v);
          VSCopy (rShift, -0.25, cellWid);
          if (IsOdd (iDir)) {
            ++ m2v.x;
            rShift.x *= -1.;
          }
          if (IsOdd (iDir / 2)) {
            ++ m2v.y;
            rShift.y *= -1.;
          }
          if (IsOdd (iDir / 4)) {
            ++ m2v.z;
            rShift.z *= -1.;
          }
          m2 = VLinear (m2v, mpCellsN);
          EvalMpL (&le, &rShift, maxOrd);
          EvalMpProdLM (&mpCell[curLevel + 1][m2].me, &le,
             &mpCell[curLevel][m1].me, maxOrd);
        }
      }
    }
  }
}

void ComputeFarCellInt ()
{
  MpTerms le;
  VecR cMid, dr, f;
  VecI m1v;
  real u;
  int j1, m1, m1x, m1y, m1z;

  for (m1z = 0; m1z < mpCells.z; m1z ++) {
    for (m1y = 0; m1y < mpCells.y; m1y ++) {
      for (m1x = 0; m1x < mpCells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, mpCells);
        if (mpCell[maxLevel][m1].occ == 0) continue;
        VAddCon (cMid, m1v, 0.5);
        VMul (cMid, cMid, cellWid);
        VVSAdd (cMid, -0.5, region);
        DO_MP_CELL (j1, m1) {
          VSub (dr, mol[j1].r, cMid);
          EvalMpL (&le, &dr, maxOrd);
          EvalMpForce (&f, &u, &mpCell[maxLevel][m1].me, &le, maxOrd);
          VVSAdd (mol[j1].ra, - mol[j1].chg, f);
          uSum += 0.5 * mol[j1].chg * u;
        }
      }
    }
  }
}

#define HiLimI(t)  Min (m1v.t + wellSep, mpCells.t - 1)

void ComputeNearCellInt ()
{
  VecR dr, ft;
  VecI m1v, m2v;
  real qq, ri;
  int j1, j2, m1, m1x, m1y, m1z, m2, m2x, m2xLo, m2y, m2yLo, m2z;

  for (m1z = 0; m1z < mpCells.z; m1z ++) {
    for (m1y = 0; m1y < mpCells.y; m1y ++) {
      for (m1x = 0; m1x < mpCells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, mpCells);
        if (mpCell[maxLevel][m1].occ == 0) continue;
        for (m2z = m1z; m2z <= HiLimI (z); m2z ++) {
          m2yLo = (m2z == m1z) ? m1y : Max (m1y - wellSep, 0);
          for (m2y = m2yLo; m2y <= HiLimI (y); m2y ++) {
            m2xLo = (m2z == m1z && m2y == m1y) ? m1x : Max (m1x - wellSep, 0);
            for (m2x = m2xLo; m2x <= HiLimI (x); m2x ++) {
              VSet (m2v, m2x, m2y, m2z);
              m2 = VLinear (m2v, mpCells);
              if (mpCell[maxLevel][m2].occ == 0) continue;
              DO_MP_CELL (j1, m1) {
                DO_MP_CELL (j2, m2) {
                  if (m1 != m2 || j2 < j1) {
                    VSub (dr, mol[j1].r, mol[j2].r);
                    ri = 1. / VLen (dr);
                    qq = mol[j1].chg * mol[j2].chg;
                    VSCopy (ft, qq * Cube (ri), dr);
                    VVAdd (mol[j1].ra, ft);
                    VVSub (mol[j2].ra, ft);
                    uSum += qq * ri;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

void EvalMpM (MpTerms *me, VecR *v, int maxOrd)
{
  real a, a1, a2, rri;
  int j, k;

  rri = 1./ VLenSq (*v);
  me->c(0, 0) = sqrt (rri);
  me->s(0, 0) = 0.;
  for (j = 1; j <= maxOrd; j ++) {
    k = j;
    a = - (2 * k - 1) * rri;
    me->c(j, k) = a * (v->x * me->c(j - 1, k - 1) - v->y * me->s(j - 1, k - 1));
    me->s(j, k) = a * (v->y * me->c(j - 1, k - 1) + v->x * me->s(j - 1, k - 1));
    for (k = j - 1; k >= 0; k --) {
      a1 = (2 * j - 1) * v->z * rri;
      a2 = (j - 1 + k) * (j - 1 - k) * rri;
      me->c(j, k) = a1 * me->c(j - 1, k);
      me->s(j, k) = a1 * me->s(j - 1, k);
      if (k < j - 1) {
        me->c(j, k) -= a2 * me->c(j - 2, k);
        me->s(j, k) -= a2 * me->s(j - 2, k);
      }
    }
  }
}

void EvalMpL (MpTerms *le, VecR *v, int maxOrd)
{
  real a, a1, a2, rr;
  int j, k;

  rr = VLenSq (*v);
  le->c(0, 0) = 1.;
  le->s(0, 0) = 0.;
  for (j = 1; j <= maxOrd; j ++) {
    k = j;
    a = - 1. / (2 * k);
    le->c(j, k) = a * (v->x * le->c(j - 1, k - 1) - v->y * le->s(j - 1, k - 1));
    le->s(j, k) = a * (v->y * le->c(j - 1, k - 1) + v->x * le->s(j - 1, k - 1));
    for (k = j - 1; k >= 0; k --) {
      a = 1. / ((j + k) * (j - k));
      a1 = (2 * j - 1) * v->z * a;
      a2 = rr * a;
      le->c(j, k) = a1 * le->c(j - 1, k);
      le->s(j, k) = a1 * le->s(j - 1, k);
      if (k < j - 1) {
        le->c(j, k) -= a2 * le->c(j - 2, k);
        le->s(j, k) -= a2 * le->s(j - 2, k);
      }
    }
  }
}

void EvalMpProdLL (MpTerms *le1, MpTerms *le2, MpTerms *le3, int maxOrd)
{
  real s2, s3, vlc2, vlc3, vls2, vls3;
  int j1, j2, j3, k1, k2, k3;

  for (j1 = 0; j1 <= maxOrd; j1 ++) {
    for (k1 = 0; k1 <= j1; k1 ++) {
      le1->c(j1, k1) = 0.;
      le1->s(j1, k1) = 0.;
      for (j2 = 0; j2 <= j1; j2 ++) {
        j3 = j1 - j2;
        for (k2 = Max (- j2, k1 - j3); k2 <= Min (j2, k1 + j3); k2 ++) {
          k3 = k1 - k2;
          vlc2 = le2->c(j2, abs (k2));
          vls2 = le2->s(j2, abs (k2));
          if (k2 < 0) vls2 = - vls2;
          vlc3 = le3->c(j3, abs (k3));
          vls3 = le3->s(j3, abs (k3));
          if (k3 < 0) vls3 = - vls3;
          s2 = (k2 < 0 && IsOdd (k2)) ? -1. : 1.;
          s3 = (k3 < 0 && IsOdd (k3)) ? -1. : 1.;
          le1->c(j1, k1) += s2 * s3 * (vlc2 * vlc3 - vls2 * vls3);
          le1->s(j1, k1) += s2 * s3 * (vls2 * vlc3 + vlc2 * vls3);
        }
      }
    }
  }
}

void EvalMpProdLM (MpTerms *me1, MpTerms *le2, MpTerms *me3, int maxOrd)
{
  real s2, s3, vlc2, vls2, vmc3, vms3;
  int j1, j2, j3, k1, k2, k3;

  for (j1 = 0; j1 <= maxOrd; j1 ++) {
    for (k1 = 0; k1 <= j1; k1 ++) {
      me1->c(j1, k1) = 0.;
      me1->s(j1, k1) = 0.;
      for (j2 = 0; j2 <= maxOrd - j1; j2 ++) {
        j3 = j1 + j2;
        for (k2 = Max (- j2, - k1 - j3); k2 <= Min (j2, - k1 + j3); k2 ++) {
          k3 = k1 + k2;
          vlc2 = le2->c(j2, abs (k2));
          vls2 = le2->s(j2, abs (k2));
          if (k2 < 0) vls2 = - vls2;
          vmc3 = me3->c(j3, abs (k3));
          vms3 = me3->s(j3, abs (k3));
          if (k3 < 0) vms3 = - vms3;
          s2 = (k2 < 0 && IsOdd (k2)) ? -1. : 1.;
          s3 = (k3 < 0 && IsOdd (k3)) ? -1. : 1.;
          me1->c(j1, k1) += s2 * s3 * (vlc2 * vmc3 + vls2 * vms3);
          me1->s(j1, k1) += s2 * s3 * (vlc2 * vms3 - vls2 * vmc3);
        }
      }
    }
  }
}

void EvalMpForce (VecR *f, real *u, MpTerms *me, MpTerms *le, int maxOrd)
{
  VecR fc, fs;
  real a;
  int j, k;

  VZero (*f);
  for (j = 1; j <= maxOrd; j ++) {
    for (k = 0; k <= j; k ++) {
      if (k < j - 1) {
        fc.x = le->c(j - 1, k + 1);
        fc.y = le->s(j - 1, k + 1);
        fs.x = le->s(j - 1, k + 1);
        fs.y = - le->c(j - 1, k + 1);
      } else {
        fc.x = 0.;
        fc.y = 0.;
        fs.x = 0.;
        fs.y = 0.;
      }
      if (k < j) {
        fc.z = le->c(j - 1, k);
        fs.z = le->s(j - 1, k);
      } else {
        fc.z = 0.;
        fs.z = 0.;
      }
      if (k > 0) {
        fc.x -= le->c(j - 1, k - 1);
        fc.y += le->s(j - 1, k - 1);
        fc.z *= 2.;
        fs.x -= le->s(j - 1, k - 1);
        fs.y -= le->c(j - 1, k - 1);
        fs.z *= 2.;
      }
      VVSAdd (*f, me->c(j, k), fc);
      VVSAdd (*f, me->s(j, k), fs);
    }
  }
  *u = 0.;
  for (j = 0; j <= maxOrd; j ++) {
    for (k = 0; k <= j; k ++) {
      a = me->c(j, k) * le->c(j, k) + me->s(j, k) * le->s(j, k);
      if (k > 0) a *= 2.;
      *u += a;
    }
  }
}


#define WallForce(t)                                        \
   {dr = ((mol[n].r.t >= 0.) ? mol[n].r.t :                 \
        - mol[n].r.t) - 0.5 * (region.t + rCut);            \
     if (dr > - rCut) {                                     \
       if (mol[n].r.t < 0.) dr = - dr;                      \
       rri = 1. / Sqr (dr);                                 \
       rri3 = Cube (rri);                                   \
       mol[n].ra.t += 48. * rri3 * (rri3 - 0.5) * rri * dr; \
       uSum += 4. * rri3 * (rri3 - 1.) + 1.;                \
     }                                                      \
   }

void ComputeWallForces ()
{
  real dr, rri, rri3;
  int n;

  DO_MOL {
    WallForce (x);
    WallForce (y);
    WallForce (z);
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


void InitCharges ()
{
  int n;

  DO_MOL mol[n].chg = (RandR () > 0.5) ? chargeMag : - chargeMag;
}

void EvalRdf ()
{
  VecR dr;
  real deltaR, normFac, rr;
  int j1, j2, k, n, rdfType;

  if (countRdf == 0) {
    for (n = 0; n < sizeHistRdf; n ++) histRdf[0][n] = histRdf[1][n] = 0.;
  }
  deltaR = rangeRdf / sizeHistRdf;
  for (j1 = 0; j1 < nMol - 1; j1 ++) {
    for (j2 = j1 + 1; j2 < nMol; j2 ++) {
      VSub (dr, mol[j1].r, mol[j2].r);
      rr = VLenSq (dr);
      if (rr < Sqr (rangeRdf)) {
        n = sqrt (rr) / deltaR;
        rdfType = (mol[j1].chg * mol[j2].chg > 0.);
        ++ histRdf[rdfType][n];
      }
    }
  }
  ++ countRdf;
  if (countRdf == limitRdf) {
    normFac = VProd (region) / (2. * M_PI * Cube (deltaR) *
       Sqr (nMol) * countRdf);
    for (k = 0; k < 2; k ++) {
      cumRdf[k][0] = 0.;
      for (n = 1; n < sizeHistRdf; n ++)
         cumRdf[k][n] = cumRdf[k][n - 1] + histRdf[k][n];
      for (n = 0; n < sizeHistRdf; n ++) {
        histRdf[k][n] *= normFac / Sqr (n - 0.5);
        cumRdf[k][n] /= 0.5 * nMol * countRdf;
      }
    }
    PrintRdf (stdout);
    countRdf = 0;
  }
}

void PrintRdf (FILE *fp)
{
  real rb;
  int k, n;

  fprintf (fp, "rdf\n");
  for (n = 0; n < sizeHistRdf; n ++) {
    rb = (n + 0.5) * rangeRdf / sizeHistRdf;
    fprintf (fp, "%8.4f", rb);
    for (k = 0; k < 2; k ++)
       fprintf (fp, "%8.4f %8.4f", histRdf[k][n], cumRdf[k][n]);
    fprintf (fp, "\n");
  }
  fflush (fp);
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

