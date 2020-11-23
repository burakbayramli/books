
/* [[pr_13_2 - tree-code method]] */


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
  int next;
} Mol;
typedef struct {
  VecR coordSum;
  int cCur, cLast, fCell, lCell;
} TLevel;
typedef struct {
  VecR cm, midPt;
  int atomPtr, nOcc, nSub, subPtr;
} TCell;

Mol *mol;
TLevel *tLevel;
TCell *tCell;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
Prop kinEnergy, totEnergy;
real kinEnInitSum;
int stepInitlzTemp;
real distFac;
int maxCells, maxLevel, nPair;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameR (distFac),
  NameI (initUcell),
  NameI (maxLevel),
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
  ComputeForces ();
  ComputeWallForces ();
  LeapfrogStep (2);
  EvalProps ();
  if (stepCount < stepEquil) AdjustInitTemp ();
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
  kinEnInitSum = 0.;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nMol = VProd (initUcell);
  maxCells = 2 * nMol;
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (tLevel, maxLevel, TLevel);
  AllocMem (tCell, maxCells, TCell);
}

void ComputeForces ()
{
  BuildIntTree ();
  LocateIntTreeCellCm ();
  ScanIntTree ();
}

void BuildIntTree ()
{
  VecR cEdge;
  int c, cFree, m, mm, n, nOccMax, nv, p, pp;

  nv = 0;
  cFree = 1;
  tLevel[nv].fCell = 0;
  tLevel[nv].lCell = 0;
  tCell[0].nOcc = nMol;
  VZero (tCell[0].midPt);
  tCell[0].atomPtr = 0;
  DO_MOL mol[n].next = n + 1;
  mol[nMol - 1].next = -1;
  nOccMax = nMol;
  cEdge = region;
  while (nOccMax > 1) {
    ++ nv;
    if (nv > maxLevel) ErrExit (ERR_TOO_MANY_LEVELS);
    VScale (cEdge, 0.5);
    tLevel[nv].fCell = cFree;
    nOccMax = 1;
    for (c = tLevel[nv - 1].fCell; c <= tLevel[nv - 1].lCell; c ++) {
      if (tCell[c].nOcc > 1) {
        tCell[c].subPtr = cFree;
        if (cFree + 8 >= maxCells) ErrExit (ERR_TOO_MANY_CELLS);
        for (m = 0; m < 8; m ++) {
          tCell[cFree + m].nOcc = 0;
          tCell[cFree + m].atomPtr = -1;
        }
        for (p = tCell[c].atomPtr; p >= 0; p = pp) {
          m = ((mol[p].r.x >= tCell[c].midPt.x) ? 1 : 0) +
              ((mol[p].r.y >= tCell[c].midPt.y) ? 2 : 0) +
              ((mol[p].r.z >= tCell[c].midPt.z) ? 4 : 0);
          pp = mol[p].next;
          mol[p].next = tCell[cFree + m].atomPtr;
          tCell[cFree + m].atomPtr = p;
          ++ tCell[cFree + m].nOcc;
        }
        mm = 0;
        for (m = 0; m < 8; m ++) {
          if (tCell[cFree + m].nOcc > 0) {
            tCell[cFree + mm].atomPtr = tCell[cFree + m].atomPtr;
            tCell[cFree + mm].nOcc = tCell[cFree + m].nOcc;
            nOccMax = Max (nOccMax, tCell[cFree + mm].nOcc);
            VSAdd (tCell[cFree + mm].midPt, tCell[c].midPt, -0.5, cEdge);
            if (m & 1) tCell[cFree + mm].midPt.x += cEdge.x;
            if (m & 2) tCell[cFree + mm].midPt.y += cEdge.y;
            if (m & 4) tCell[cFree + mm].midPt.z += cEdge.z;
            ++ mm;
          }
        }
        tCell[c].nSub = mm;
        cFree += mm;
      }
    }
    tLevel[nv].lCell = cFree - 1;
  }
  tLevel[nv + 1].lCell = -1;
}

void LocateIntTreeCellCm ()
{
  int nv, p;

  nv = 0;
  tLevel[nv].cCur = 0;
  tLevel[nv].cLast = tCell[0].nSub;
  VZero (tLevel[nv].coordSum);
  do {
    if (tLevel[nv].cCur < tLevel[nv].cLast) {
      if (tCell[tLevel[nv].cCur].nOcc == 1) {
        p = tCell[tLevel[nv].cCur].atomPtr;
        tCell[tLevel[nv].cCur].cm = mol[p].r;
        VVAdd (tLevel[nv].coordSum, mol[p].r);
        ++ tLevel[nv].cCur;
      } else {
        ++ nv;
        tLevel[nv].cCur = tCell[tLevel[nv - 1].cCur].subPtr;
        tLevel[nv].cLast = tLevel[nv].cCur + tCell[tLevel[nv - 1].cCur].nSub;
        VZero (tLevel[nv].coordSum);
      }
    } else {
      VVAdd (tLevel[nv - 1].coordSum, tLevel[nv].coordSum);
      VSCopy (tCell[tLevel[nv - 1].cCur].cm, 1. / tCell[tLevel[nv - 1].cCur].nOcc,
         tLevel[nv].coordSum);
      -- nv;
      ++ tLevel[nv].cCur;
    }
  } while (nv > 0);
}

void ScanIntTree ()
{
  VecR dr;
  real b, edgeLen, rr, rri;
  int n, nv, p;

  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  nPair = 0;
  DO_MOL {
    nv = 0;
    edgeLen = region.x;
    tLevel[nv].cCur = 0;
    tLevel[nv].cLast = tCell[0].nSub;
    do {
      if (tLevel[nv].cCur < tLevel[nv].cLast) {
        VSub (dr, mol[n].r, tCell[tLevel[nv].cCur].cm);
        rr = VLenSq (dr);
        if (rr > Sqr (distFac * edgeLen)) {
          rri = 1. / rr;
          b = tCell[tLevel[nv].cCur].nOcc * sqrt (rri);
          VVSAdd (mol[n].ra, b * rri, dr);
          uSum += b;
          ++ nPair;
          ++ tLevel[nv].cCur;
        } else {
          if (tCell[tLevel[nv].cCur].nOcc == 1) {
            p = tCell[tLevel[nv].cCur].atomPtr;
            if (p != n) {
              VSub (dr, mol[n].r, mol[p].r);
              rri = 1. / VLenSq (dr);
              b = sqrt (rri);
              VVSAdd (mol[n].ra, b * rri, dr);
              uSum += b;
              ++ nPair;
            }
            ++ tLevel[nv].cCur;
          } else {
            ++ nv;
            tLevel[nv].cCur = tCell[tLevel[nv - 1].cCur].subPtr;
            tLevel[nv].cLast = tLevel[nv].cCur + tCell[tLevel[nv - 1].cCur].nSub;
            edgeLen *= 0.5;
        }
      }
      } else {
        -- nv;
        ++ tLevel[nv].cCur;
        edgeLen *= 2.;
      }
    } while (nv > 0);
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
  DO_MOL {
    VVAdd (vSum, mol[n].rv);
    vv = VLenSq (mol[n].rv);
    vvSum += vv;
  }
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

