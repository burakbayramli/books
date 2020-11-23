
/* [[pr_09_2 - surfactant model]] */


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
  int inChain, inClust, typeA;
} Mol;
typedef struct {
  int head, next, size;
} Clust;

Mol *mol;
Clust *clust;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, runId, stepAvg, stepCount, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
int stepAdjustTemp;
real bondLim, rCutA, solConc;
int *intType, chainHead, chainLen, nChain;
real rClust;
int bigSize, nClust, nSingle;
Prop cSize;
int stepSnap;

NameList nameList[] = {
  NameR (bondLim),
  NameI (chainHead),
  NameI (chainLen),
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (nebrTabFac),
  NameI (nChain),
  NameR (rClust),
  NameR (rCutA),
  NameR (rNebrShell),
  NameI (runId),
  NameR (solConc),
  NameI (stepAdjustTemp),
  NameI (stepAvg),
  NameI (stepLimit),
  NameI (stepSnap),
  NameR (temperature),
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
  ApplyBoundaryCond ();
  if (nebrNow) {
    nebrNow = 0;
    dispHi = 0.;
    BuildNebrList ();
  }
  ComputeForces ();
  ComputeChainBondForces ();
  LeapfrogStep (2);
  EvalProps ();
  if (stepCount % stepAdjustTemp == 0) AdjustTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    BuildClusters ();
    CompressClusters ();
    AnalClusterSize ();
    PrintSummary (stdout);
    AccumProps (0);
    InitClusters ();
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
  InitClusters ();
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nMol = VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / (rCutA + rNebrShell), region);
  rClust = Min (rClust, rCutA + rNebrShell);
  nebrTabMax = nebrTabFac * nMol;
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
  AllocMem (intType, nebrTabMax, int);
  AllocMem (clust, nMol, Clust);
}

void BuildNebrList ()
{
  VecR dr, invWid, rs, shift;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrNebr, rrNebrA;
  int c, iType, j1, j2, m1, m1x, m1y, m1z, m2, n, offset, sameChain;

  rrNebr = Sqr (rCut + rNebrShell);
  rrNebrA = Sqr (rCutA + rNebrShell);
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
                sameChain = (mol[j1].inChain == mol[j2].inChain &&
                   mol[j1].inChain >= 0);
                iType = 0;
                if (mol[j1].typeA == mol[j2].typeA && ! sameChain) {
                  if (VLenSq (dr) < rrNebrA) iType = 2;
                } else if (! sameChain || abs (j1 - j2) > 1) {
                  if (VLenSq (dr) < rrNebr) iType = 1;
                }
                if (iType > 0) {
                  if (nebrTabLen >= nebrTabMax)
                     ErrExit (ERR_TOO_MANY_NEBRS);
                  nebrTab[2 * nebrTabLen] = j1;
                  nebrTab[2 * nebrTabLen + 1] = j2;
                  intType[nebrTabLen] = iType;
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
  real fcVal, rr, rrCut, rrCutA, rri, rri3, uVal;
  int j1, j2, n;

  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  rrCut = Sqr (rCut);
  rrCutA = Sqr (rCutA);
  for (n = 0; n < nebrTabLen; n ++) {
    j1 = nebrTab[2 * n];
    j2 = nebrTab[2 * n + 1];
    VSub (dr, mol[j1].r, mol[j2].r);
    VWrapAll (dr);
    rr = VLenSq (dr);
    if (rr < rrCut || intType[n] == 2 && rr < rrCutA) {
      rri = 1. / rr;
      rri3 = Cube (rri);
      fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
      uVal = 4. * rri3 * (rri3 - 1.);
      if (intType[n] == 1) uVal += 1.;
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


void InitCoords ()
{
  VecR c, gap;
  VecI cc;
  int *initSiteOcc, dir, j, n, nc, nn, nx, ny, nz;

  AllocMem (initSiteOcc, nMol, int);
  DO_MOL initSiteOcc[n] = 0;
  for (nc = 0; nc < nChain; nc ++) {
    while (1) {
      VSet (cc, RandR () * (initUcell.x - chainLen),
         RandR () * initUcell.y, RandR () * initUcell.z);
      n = VLinear (cc, initUcell);
      for (j = 0; j < chainLen; j ++) {
        if (initSiteOcc[n + j]) break;
      }
      if (j == chainLen) {
        for (j = 0; j < chainLen; j ++) initSiteOcc[n + j] = 1;
        break;
      }
    }
  }
  VDiv (gap, region, initUcell);
  nc = 0;
  n = 0;
  nn = 0;
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.5, ny + 0.5, nz + 0.5);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, region);
        if (initSiteOcc[nn]) {
          dir = (RandR () < 0.5) ? 0 : 1;
          for (j = 0; j < chainLen; j ++) {
            mol[n].r = c;
            mol[n].r.x += (dir ? j : chainLen - 1 - j) * rCut;
            mol[n].typeA = (j >= chainHead) ? 1 : 2;
            mol[n].inChain = nc;
            ++ n;
          }
          nx += chainLen - 1;
          ++ nc;
          nn += chainLen;
        } else ++ nn;
      }
    }
  }
  nn = 0;
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.5, ny + 0.5, nz + 0.5);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, region);
        if (initSiteOcc[nn]) {
          nx += chainLen - 1;
          nn += chainLen;
        } else {
          mol[n].r = c;
          mol[n].typeA = (RandR () < solConc) ? 1 : 2;
          mol[n].inChain = -1;
          ++ n;
          ++ nn;
        }
      }
    }
  }
  free (initSiteOcc);
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


void AdjustTemp ()
{
  real vFac;
  int n;

  vvSum = 0.;
  DO_MOL vvSum += VLenSq (mol[n].rv);
  vFac = velMag / sqrt (vvSum / nMol);
  DO_MOL VScale (mol[n].rv, vFac);
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


void BuildClusters ()
{
  VecR dr, invWid, rs, shift;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrClust;
  int c, j1, j2, m1, m1x, m1y, m1z, m2, n, offset;

  rrClust = Sqr (rClust);
  VDiv (invWid, cells, region);
  for (n = nMol; n < nMol + VProd (cells); n ++) cellList[n] = -1;
  DO_MOL {
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nMol;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
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
              if ((m1 != m2 || j2 < j1) && mol[j1].typeA == 1 &&
                 mol[j2].typeA == 1) {
                VSub (dr, mol[j1].r, mol[j2].r);
                VVSub (dr, shift);
                if (VLenSq (dr) < rrClust) AddBondedPair (j1, j2);
              }
            }
          }
        }
      }
    }
  }
}


void AddBondedPair (int j1, int j2)
{
  int cBig, cSmall, m, mp, nc1, nc2;

  nc1 = mol[j1].inClust;
  nc2 = mol[j2].inClust;
  if (nc1 < 0 && nc2 < 0) {
    mol[j1].inClust = nClust;
    mol[j2].inClust = nClust;
    clust[nClust].size = 2;
    clust[nClust].head = j1;
    clust[j1].next = j2;
    clust[j2].next = -1;
    ++ nClust;
  } else if (mol[j1].inClust < 0) {
    mol[j1].inClust = nc2;
    clust[j1].next = clust[nc2].head;
    clust[nc2].head = j1;
    ++ clust[nc2].size;
  } else if (mol[j2].inClust < 0) {
    mol[j2].inClust = nc1;
    clust[j2].next = clust[nc1].head;
    clust[nc1].head = j2;
    ++ clust[nc1].size;
  } else {
    if (nc1 != nc2) {
      cBig = (clust[nc1].size > clust[nc2].size) ? nc1 : nc2;
      cSmall = nc1 + nc2 - cBig;
      for (m = clust[cSmall].head; m >= 0; m = clust[m].next) {
        mol[m].inClust = cBig;
        mp = m;
      }
      clust[mp].next = clust[cBig].head;
      clust[cBig].head = clust[cSmall].head;
      clust[cBig].size += clust[cSmall].size;
      clust[cSmall].size = 0;
    }
  }
}

void InitClusters ()
{
  int n;

  DO_MOL mol[n].inClust = -1;
  nClust = 0;
}

void CompressClusters ()
{
  int j, m, nc;

  nc = 0;
  for (j = 0; j < nClust; j ++) {
    if (clust[j].size > 0) {
      clust[nc].head = clust[j].head;
      clust[nc].size = clust[j].size;
      for (m = clust[nc].head; m >= 0; m = clust[m].next)
         mol[m].inClust = nc;
      ++ nc;
    }
  }
  nClust = nc;
}

void AnalClusterSize ()
{
  int cBig, nc, ncUse;

  PropZero (cSize);
  ncUse = 0;
  cBig = 0;
  for (nc = 0; nc < nClust; nc ++) {
    cSize.val = clust[nc].size;
    if (cSize.val > clust[cBig].size) cBig = nc;
    if (cSize.val > 1) {
      ++ ncUse;
      PropAccum (cSize);
    }
  }
  bigSize = clust[cBig].size;
  nSingle = nMol - cSize.sum;
  if (ncUse > 0) PropAvg (cSize, ncUse);
}


void PrintSummary (FILE *fp)
{
  fprintf (fp,
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy));
  fprintf (fp, "  %d %d %d %.1f %.1f", nSingle, nClust, bigSize,
     PropEst (cSize));
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
  blockSize = (NDIM + 1) * sizeof (real) + 5 * sizeof (int) +
     nMol * (NDIM + 1) * sizeof (short);
  if ((fp = fopen (fileName[FL_SNAP], "a")) != 0) {
    WriteF (blockSize);
    WriteF (chainLen);
    WriteF (nChain);
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
    DO_MOL rI[n] = ((mol[n].inChain + 1) << 2) + mol[n].typeA;
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

