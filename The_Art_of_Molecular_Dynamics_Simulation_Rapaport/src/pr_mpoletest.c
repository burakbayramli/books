
/* [[pr_mpoletest - test fast-multipole sums]] */


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
  VecR r, ra;
  real chg;
} Mol;

MpCell **mpCell;
Mol *mol;
VecR *raD, cellWid, region;
VecI mpCells;
real uSum, uSumD;
int *mpCellList, curCellsEdge, curLevel, maxCellsEdge, maxLevel, maxOrd, nMol,
   randSeed, wellSep;

#define TIMING  0

#define TDIFF(tv2, tv1)                                     \
   (1e6 * (tv2.tv_sec - tv1.tv_sec) + tv2.tv_usec - tv1.tv_usec)
#define TIME_DATA  struct timeval tv1, tv2
#define TimeStart(d)  gettimeofday (&tv1, NULL)
#define TimeEnd(t)                                          \
   gettimeofday (&tv2, NULL);                               \
   t = TDIFF (tv2, tv1) / 1e6;
real tm[4];

int main ()
{
  VecR dr, dra, ft, raSum, raSumD;
  Prop uErr, aErr;
  real qq, ri;
  int j1, j2, n, nEst;
  int doDirect;
  TIME_DATA;

  nMol = 8000;
  maxOrd = 2 /*MAX_MPEX_ORD*/;
  maxLevel = 3;
  wellSep = 1;
  AllocArrays ();
  VSetAll (region, 1.);
#if TIMING
  doDirect = 1;
#endif
  nEst = 0;
  PropZero (uErr);
  PropZero (aErr);
#if TIMING
  randSeed = 17;
  if (1) {
#else
  for (randSeed = 17; randSeed <= 21; randSeed ++) {
#endif
    InitRand (randSeed);
    printf ("%d %d %d %d %d\n", maxLevel, wellSep, maxOrd, nMol, randSeed);
    InitCoords ();
    InitCharges ();
    InitAccels ();
    uSum = 0.;
    MultipoleCalc ();
    VZero (raSum);
    DO_MOL {
      raSum.x += fabs (mol[n].ra.x);
      raSum.y += fabs (mol[n].ra.y);
      raSum.z += fabs (mol[n].ra.z);
    }
    printf ("%.8e %.8e ", uSum / nMol, VLen (raSum) / nMol);
    DO_MOL VZero (raD[n]);
    uSumD = 0.;
#if TIMING
    TimeStart ();
    if (doDirect) {
#else
    if (1) {
#endif
      for (j1 = 0; j1 < nMol; j1 ++) {
        for (j2 = 0; j2 < j1; j2 ++) {
          VSub (dr, mol[j1].r, mol[j2].r);
          ri = 1. / VLen (dr);
          qq = mol[j1].chg * mol[j2].chg;
          VSCopy (ft, qq * Cube (ri), dr);
          VVAdd (raD[j1], ft);
          VVSub (raD[j2], ft);
          uSumD += qq * ri;
        }
      }
    }
#if TIMING
    TimeEnd (tm[3]);
#endif
#if TIMING
    if (doDirect) {
#else
    if (1) {
#endif
      VZero (raSumD);
      DO_MOL {
        raSumD.x += fabs (raD[n].x);
        raSumD.y += fabs (raD[n].y);
        raSumD.z += fabs (raD[n].z);
      }
      VSub (dra, raSum, raSumD);
      uErr.val = fabs (uSum / uSumD - 1.);
      aErr.val = VLen (dra) / VLen (raSumD);
      ++ nEst;
      PropAccum (uErr);
      PropAccum (aErr);
      printf ("%.8e %.8e", uErr.val, aErr.val);
      printf ("\n");
    }
  }
  if (nEst > 1) {
    PropAvg (uErr, nEst);
    PropAvg (aErr, nEst);
    printf ("Av err: %.5e %.5e %.5e %.5e\n", PropEst (uErr), PropEst (aErr));
  }
#if TIMING
  printf ("Time: (%.3f %.3f %.3f) %.3f %.3f\n",
     tm[0], tm[1], tm[2], tm[0] + tm[1] + tm[2], tm[3]);
#endif
}

void AllocArrays ()
{
  int n;

  AllocMem (mol, nMol, Mol);
  AllocMem (raD, nMol, VecR);
  AllocMem (mpCell, maxLevel + 1, MpCell *);
  maxCellsEdge = 2;
  for (n = 2; n <= maxLevel; n ++) {
    maxCellsEdge *= 2;
    VSetAll (mpCells, maxCellsEdge);
    AllocMem (mpCell[n], VProd (mpCells), MpCell);
  }
  AllocMem (mpCellList, nMol + VProd (mpCells), int);
}

void InitCoords ()
{
  int n;

  DO_MOL {
    VSet (mol[n].r, RandR (), RandR (), RandR ());
    VAddCon (mol[n].r, mol[n].r, - 0.5);
    VMul (mol[n].r, mol[n].r, region);
  }
}

void InitCharges ()
{
  int n;

  DO_MOL mol[n].chg = 1.;
}


void MultipoleCalc ()
{
  int j, k, m1;
  real t;
  TIME_DATA;

  TimeStart ();
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
  TimeEnd (tm[0]);
  TimeStart ();
  for (m1 = 0; m1 < 64; m1 ++) {
    for (j = 0; j <= maxOrd; j ++) {
      for (k = 0; k <= j; k ++) {
        mpCell[2][m1].me.c(j, k) = 0.;
        mpCell[2][m1].me.s(j, k) = 0.;
      }
    }
  }
  tm[1] = 0.;
  curCellsEdge = 2;
  for (curLevel = 2; curLevel <= maxLevel; curLevel ++) {
    TimeStart ();
    curCellsEdge *= 2;
    VSetAll (mpCells, curCellsEdge);
    VDiv (cellWid, region, mpCells);
    GatherWellSepLo ();
    TimeEnd (t);
    tm[1] += t;
    TimeStart ();
    if (curLevel < maxLevel) PropagateCellLo ();
    TimeEnd (t);
    tm[0] += t;
  }
  TimeStart ();
  ComputeFarCellInt ();
  TimeEnd (t);
  tm[0] += t;
  TimeStart ();
  ComputeNearCellInt ();
  TimeEnd (tm[2]);
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


void InitAccels ()
{
  int n;

  DO_MOL VZero (mol[n].ra);
}


#include "in_rand.c"

