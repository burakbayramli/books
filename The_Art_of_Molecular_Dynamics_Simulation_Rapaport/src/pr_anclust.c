
/* [[pr_anclust - cluster analysis]] */


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
  VecR r;
  int inClust;
} Mol;
typedef struct {
  int head, next, size;
} Clust;

Mol *mol;
Clust *clust;
VecR region;
VecI cells;
real rClust, timeNow;
Prop cSize;
int *cellList, bigSize, blockNum, blockSize, nMol, nCellEdge, nClust, nSingle,
   runId, stepCount;
FILE *fp;

int main (int argc, char **argv)
{
  runId = atoi (argv[1]);
  rClust = atof (argv[2]);
  printf ("id: %d  rClust: %.3f\n", runId, rClust);
  SetupFiles ();
  blockNum = -1;
  while (GetConfig ()) {
    InitClusters ();
    BuildClusters ();
    CompressClusters ();
    AnalClusterSize ();
    printf ("%d %d %d %.1f %.1f\n", nSingle, nClust, bigSize, PropEst (cSize));
    fflush (stdout);
  }
}

void SetCellSize ()
{
  nCellEdge = region.x / rClust;
  VSetAll (cells, nCellEdge);
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (clust, nMol, Clust);
  AllocMem (cellList, Cube (nCellEdge) + nMol, int);
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
              if (m1 != m2 || j2 < j1) {
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


#define SCALE_FAC  32767.

int GetConfig ()
{
  VecR w;
  int fOk, n;
  short *rI;

  fOk = 1;
  if (blockNum == -1) {
    if ((fp = fopen (fileName[FL_SNAP], "r")) == 0) fOk = 0;
  } else {
    fseek (fp, blockNum * blockSize, 0);
    ++ blockNum;
  }
  if (fOk) {
    ReadF (blockSize);
    if (feof (fp)) return (0);
    ReadF (nMol);
    ReadF (region);
    ReadF (stepCount);
    ReadF (timeNow);
    if (blockNum == -1) {
      SetCellSize ();
      AllocArrays ();
      blockNum = 1;
    }
    AllocMem (rI, NDIM * nMol, short);
    ReadFN (rI, NDIM * nMol);
    DO_MOL {
      VFromLin (w, rI, NDIM * n);
      VScale (w, 1. / SCALE_FAC);
      VAddCon (w, w, -0.5);
      VMul (mol[n].r, w, region);
    }
    free (rI);
    if (ferror (fp)) fOk = 0;
  }
  if (! fOk) ErrExit (ERR_SNAP_READ);
  return (1);
}

void SetupFiles ()
{
  strcpy (fileName[FL_SNAP], fileNameR[FL_SNAP]);
  fileName[FL_SNAP][0] = progId[0];
  fileName[FL_SNAP][1] = progId[1];
  fileName[FL_SNAP][2] = runId / 10 + CHAR_ZERO;
  fileName[FL_SNAP][3] = runId % 10 + CHAR_ZERO;
}


#include "in_errexit.c"

