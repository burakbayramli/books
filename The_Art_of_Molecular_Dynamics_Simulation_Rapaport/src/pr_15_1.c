
/* [[pr_15_1 - hard-disk thermal convection]] */


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


#define NDIM  2

#include "in_mddefs.h"

typedef struct {
  VecR r, rv;
  real time;
  VecI inCell;
} Mol;

Mol *mol;
EvTree *evTree;
VecR region, vSum;
VecI cellRange[2], cells, initUcell;
real collCount, crossCount, density, intervalSum, kinEnVal, nextSumTime,
   temperature, timeNow, velMag;
int *cellList, eventCount, eventMult, evIdA, evIdB, limitEventCount, moreCycles,
   nMol, poolSize, runId;
VecR gravField;
real totEnVal;
VecR roughWid;
real wallTemp[2];
int thermalWall[NDIM];
VecI sizeHistGrid;
real **histGrid, *profileD, *profileT, intervalGrid, nextSnapTime;
int countGrid, limitGrid, snapNumber;

NameList nameList[] = {
  NameR (density),
  NameI (eventMult),
  NameR (gravField),
  NameI (initUcell),
  NameR (intervalGrid),
  NameR (intervalSum),
  NameI (limitEventCount),
  NameI (limitGrid),
  NameI (runId),
  NameI (sizeHistGrid),
  NameR (temperature),
  NameI (thermalWall),
  NameR (wallTemp),
};


int main (int argc, char **argv)
{
  GetNameList (argc, argv);
  PrintNameList (stdout);
  SetParams ();
  SetupJob ();
  moreCycles = 1;
  eventCount = 0;
  while (moreCycles) {
    SingleEvent ();
    ++ eventCount;
    if (eventCount >= limitEventCount) moreCycles = 0;
  }
}


void SetupJob ()
{
  SetupFiles ();
  AllocArrays ();
  InitCoords ();
  InitVels ();
  timeNow = nextSumTime = 0.;
  collCount = crossCount = 0.;
  countGrid = 0;
  nextSnapTime = intervalGrid;
  StartRun ();
  GridAverage (0);
  ScheduleEvent (0, MOL_LIMIT + 6, nextSumTime);
  ScheduleEvent (0, MOL_LIMIT + 7, nextSnapTime);
}

void SingleEvent ()
{
  real vvSum;
  real sp;
  int n;

  NextEvent ();
  if (evIdB < MOL_LIMIT) {
    ProcessCollision ();
    ++ collCount;
  } else if (evIdB < MOL_LIMIT + NDIM * 2 || evIdB >= MOL_LIMIT + 100) {
    ProcessCellCrossing ();
    ++ crossCount;
  } else if (evIdB == MOL_LIMIT + 6) {
    UpdateSystem ();
    nextSumTime += intervalSum;
    ScheduleEvent (0, MOL_LIMIT + 6, nextSumTime);
    VZero (vSum);
    vvSum = 0.;
    sp = 0.;
    DO_MOL {
      VVAdd (vSum, mol[n].rv);
      vvSum += VLenSq (mol[n].rv);
      sp += VDot (mol[n].r, gravField);
    }
    kinEnVal = vvSum * 0.5 / nMol;
    totEnVal = kinEnVal - sp / nMol;
    PrintSummary (stdout);
  } else if (evIdB == MOL_LIMIT + 7) {
    UpdateSystem ();
    nextSnapTime += intervalGrid;
    ScheduleEvent (0, MOL_LIMIT + 7, nextSnapTime);
    GridAverage (1);
    ++ countGrid;
    if (countGrid % limitGrid == 0) {
      snapNumber = countGrid / limitGrid;
      GridAverage (2);
      PutGridAverage ();
      EvalProfile ();
      GridAverage (0);
      PrintProfile (stdout);
    }
  }
}

void SetParams ()
{
  VecI iw;

  VSCopy (region, 1. / sqrt (density), initUcell);
  nMol = VProd (initUcell);
  VAddCon (region, region, 1.);
  VCopy (iw, region);
  VDiv (roughWid, region, iw);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  poolSize = eventMult * nMol;
  VCopy (cells, region);
}

void AllocArrays ()
{
  int k;

  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (evTree, poolSize, EvTree);
  AllocMem2 (histGrid, NHIST, VProd (sizeHistGrid), real);
  AllocMem (profileT, VProd (sizeHistGrid), real);
  AllocMem (profileD, VProd (sizeHistGrid), real);
}

void PrintSummary (FILE *fp)
{
  fprintf (fp, "%.2f %.10g %.10g %.3f",
     timeNow, collCount, crossCount, VCSum (vSum) / nMol);
  fprintf (fp, " %.3f", totEnVal);
  fprintf (fp, "\n");
  fflush (fp);
}

void InitCoords ()
{
  VecR c, gap, offset;
  int n, nx, ny;

  VAddCon (gap, region, -1.);
  VDiv (gap, gap, initUcell);
  VSetAll (offset, 0.5);
  n = 0;
  for (ny = 0; ny < initUcell.y; ny ++) {
    for (nx = 0; nx < initUcell.x; nx ++) {
      VSet (c, nx + 0.5, ny + 0.5);
      VMul (c, c, gap);
      VVSAdd (c, -0.5, region);
      VVAdd (c, offset);
      mol[n].r = c;
      ++ n;
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


void ProcessCollision ()
{
  VecR dr, dv;
  real fac;

  UpdateMol (evIdA);
  UpdateMol (evIdB);
  VSetAll (cellRange[0], -1);
  VSetAll (cellRange[1], 1);
  VSub (dr, mol[evIdA].r, mol[evIdB].r);
  VSub (dv, mol[evIdA].rv, mol[evIdB].rv);
  fac = - VDot (dr, dv) / VLenSq (dr);
  VVSAdd (mol[evIdA].rv, fac, dr);
  VVSAdd (mol[evIdB].rv, - fac, dr);
  PredictEvent (evIdA, -1);
  PredictEvent (evIdB, evIdA);
}

#define VWrapEvC(t)                         \
   if (mol[evIdA].rv.t > 0.) {              \
     cellRange[0].t = 1;                    \
     ++ mol[evIdA].inCell.t;                \
     if (mol[evIdA].inCell.t == cells.t) {  \
       mol[evIdA].inCell.t = 0;             \
       mol[evIdA].r.t = -0.5 * region.t;    \
     }                                      \
   } else {                                 \
     cellRange[1].t = -1;                   \
     -- mol[evIdA].inCell.t;                \
     if (mol[evIdA].inCell.t == -1) {       \
       mol[evIdA].inCell.t = cells.t - 1;   \
       mol[evIdA].r.t = 0.5 * region.t;     \
     }                                      \
   }

void ProcessCellCrossing ()
{
  VecR rs;
  VecI irs;
  real vFac, vv;
  int j, jj, n;

  UpdateMol (evIdA);
  n = VLinear (mol[evIdA].inCell, cells) + nMol;
  while (cellList[n] != evIdA) n = cellList[n];
  cellList[n] = cellList[evIdA];
  VSetAll (cellRange[0], -1);
  VSetAll (cellRange[1], 1);
  j = evIdB - MOL_LIMIT;
  if (j >= 100) {
    jj = j - 100;
    switch (jj) {
      case 0:
        VWrapEvC (x);
        break;
      case 1:
        VWrapEvC (y);
        break;
    }
  } else {
    jj = j / 2;
    VComp (cellRange[j % 2], jj) = 0;
    VComp (mol[evIdA].rv, jj) *= -1.;
    VSAdd (rs, mol[evIdA].r, 0.5, region);
    VDiv (rs, rs, roughWid);
    VCopy (irs, rs);
    if (jj != 0 && irs.x % 2 == 0) mol[evIdA].rv.x *= -1.;
    if (jj != 1 && irs.y % 2 == 0) mol[evIdA].rv.y *= -1.;
    if (thermalWall[jj]) {
      vv = VLenSq (mol[evIdA].rv);
      vFac = sqrt (NDIM * wallTemp[j % 2] / vv);
      VScale (mol[evIdA].rv, vFac);
    }
  }
  PredictEvent (evIdA, evIdB);
  n = VLinear (mol[evIdA].inCell, cells) + nMol;
  cellList[evIdA] = cellList[n];
  cellList[n] = evIdA;
}

#define WhenCross(t)                                        \
   tm.t = (mol[na].rv.t != 0.) ? w.t / mol[na].rv.t : 1e12;

#undef VWrapEvC

#define VWrapEvC(t)                                         \
   m2v.t = mol[na].inCell.t + m1v.t;

#define LimitCells(t)                                       \
  if (mol[na].inCell.t + cellRangeT[0].t == -1)             \
     cellRangeT[0].t = 0;                                   \
  if (mol[na].inCell.t + cellRangeT[1].t == cells.t)        \
     cellRangeT[1].t = 0;

void PredictEvent (int na, int nb)
{
  VecR dr, dv, rs, tm, w;
  VecI cellRangeT[2], m1v, m2v, signDir;
  real b, d, h, h1, h2, t, tInt, vv;
  int dir, evCode, m1x, m1y, n;

  VCopy (w, mol[na].inCell);
  if (mol[na].rv.x > 0.) ++ w.x;
  signDir.x = (mol[na].rv.x < 0.);
  if (gravField.y == 0. && mol[na].rv.y > 0.) ++ w.y;
  VMul (w, w, region);
  VDiv (w, w, cells);
  VSAdd (rs, mol[na].r, 0.5, region);
  VVSub (w, rs);
  WhenCross (x);
  if (gravField.y != 0.) {
    h1 = Sqr (mol[na].rv.y) + 2. * gravField.y * w.y;
    if (mol[na].rv.y > 0.) {
      h2 = h1 + 2. * gravField.y * region.y / cells.y;
      if (h2 > 0.) {
        h = - sqrt (h2);
        signDir.y = 0;
      } else {
        h = sqrt (h1);
        signDir.y = 1;
      }
    } else {
      h = sqrt (h1);
      signDir.y = 1;
    }
    tm.y = - (mol[na].rv.y + h) / gravField.y;
  } else {
    WhenCross (y);
    signDir.y = (mol[na].rv.y < 0.);
  }
  dir = (tm.x < tm.y) ? 0 : 1;
  evCode = 100 + dir;
  if (VComp (mol[na].inCell, dir) == 0 &&
     VComp (signDir, dir) == 1) evCode = 2 * dir;
  else if (VComp (mol[na].inCell, dir) == VComp (cells, dir) - 1 &&
     VComp (signDir, dir) == 0) evCode = 2 * dir + 1;
  ScheduleEvent (na, MOL_LIMIT + evCode, timeNow + VComp (tm, dir));
  cellRangeT[0] = cellRange[0];
  cellRangeT[1] = cellRange[1];
  LimitCells (x);
  LimitCells (y);
  for (m1y = cellRangeT[0].y; m1y <= cellRangeT[1].y; m1y ++) {
    m1v.y = m1y;
    VWrapEvC (y);
    for (m1x = cellRangeT[0].x; m1x <= cellRangeT[1].x; m1x ++) {
      m1v.x = m1x;
      VWrapEvC (x);
      n = VLinear (m2v, cells) + nMol;
      for (n = cellList[n]; n >= 0; n = cellList[n]) {
        if (n != na && n != nb && (nb >= -1 || n < na)) {
          tInt = timeNow - mol[n].time;
          VSub (dr, mol[na].r, mol[n].r);
          VVSAdd (dr, - tInt, mol[n].rv);
          VVSAdd (dr, -0.5 * Sqr (tInt), gravField);
          VSub (dv, mol[na].rv, mol[n].rv);
          VVSAdd (dv, - tInt, gravField);
          b = VDot (dr, dv);
          if (b < 0.) {
            vv = VLenSq (dv);
            d = Sqr (b) - vv * (VLenSq (dr) - 1.);
            if (d >= 0.) {
              t = - (sqrt (d) + b) / vv;
              ScheduleEvent (na, n, timeNow + t);
            }
          }
        }
      }
    }
  }
}


void StartRun ()
{
  VecR rs;
  int j, n;

  for (j = 0; j < VProd (cells) + nMol; j ++) cellList[j] = -1;
  DO_MOL {
    mol[n].time = timeNow;
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (rs, rs, cells);
    VDiv (mol[n].inCell, rs, region);
    j = VLinear (mol[n].inCell, cells) + nMol;
    cellList[n] = cellList[j];
    cellList[j] = n;
  }
  InitEventList ();
  VSetAll (cellRange[0], -1);
  VSetAll (cellRange[1], 1);
  DO_MOL PredictEvent (n, -2);
}


void UpdateMol (int id)
{
  real tInt;

  tInt = timeNow - mol[id].time;
  VVSAdd (mol[id].r, tInt, mol[id].rv);
  VVSAdd (mol[id].r, 0.5 * Sqr (tInt), gravField);
  VVSAdd (mol[id].rv, tInt, gravField);
  mol[id].time = timeNow;
}

void UpdateSystem ()
{
  int n;

  DO_MOL UpdateMol (n);
}


void ScheduleEvent (int idA, int idB, real tEvent)
{
  int id, idNew, more;

  id = 0;
  if (idB < MOL_LIMIT ||
     idB >= MOL_LIMIT + 2 * NDIM && idB < MOL_LIMIT + 100) {
    if (evTree[0].idA < 0) ErrExit (ERR_EMPTY_EVPOOL);
    idNew = evTree[0].idA;
    evTree[0].idA = evTree[evTree[0].idA].circAR;
  } else idNew = idA + 1;
  if (evTree[id].right < 0) evTree[id].right = idNew;
  else {
    more = 1;
    id = evTree[id].right;
    while (more) {
      if (tEvent < evTree[id].time) {
        if (evTree[id].left >= 0) id = evTree[id].left;
        else {
          more = 0;
          evTree[id].left = idNew;
        }
      } else {
        if (evTree[id].right >= 0) id = evTree[id].right;
        else {
          more = 0;
          evTree[id].right = idNew;
        }
      }
    }
  }
  if (idB < MOL_LIMIT) {
    evTree[idNew].circAR = evTree[idA + 1].circAR;
    evTree[idNew].circAL = idA + 1;
    evTree[evTree[idA + 1].circAR].circAL = idNew;
    evTree[idA + 1].circAR = idNew;
    evTree[idNew].circBR = evTree[idB + 1].circBR;
    evTree[idNew].circBL = idB + 1;
    evTree[evTree[idB + 1].circBR].circBL = idNew;
    evTree[idB + 1].circBR = idNew;
  }
  evTree[idNew].time = tEvent;
  evTree[idNew].idA = idA;
  evTree[idNew].idB = idB;
  evTree[idNew].left = evTree[idNew].right = -1;
  evTree[idNew].up = id;
}

void NextEvent ()
{
  int idNow;

  idNow = evTree[0].right;
  while (evTree[idNow].left >= 0) idNow = evTree[idNow].left;
  timeNow = evTree[idNow].time;
  evIdA = evTree[idNow].idA;
  evIdB = evTree[idNow].idB;
  if (evIdB < MOL_LIMIT + 2 * NDIM) {
    DeleteAllMolEvents (evIdA);
    if (evIdB < MOL_LIMIT) DeleteAllMolEvents (evIdB);
  } else {
    DeleteEvent (idNow);
    if (evIdB < MOL_LIMIT + 100) {
      evTree[idNow].circAR = evTree[0].idA;
      evTree[0].idA = idNow;
    }
  }
}

void DeleteAllMolEvents (int id)
{
  int idd;

  ++ id;
  DeleteEvent (id);
  for (idd = evTree[id].circAL; idd != id; idd = evTree[idd].circAL) {
    evTree[evTree[idd].circBL].circBR = evTree[idd].circBR;
    evTree[evTree[idd].circBR].circBL = evTree[idd].circBL;
    DeleteEvent (idd);
  }
  evTree[evTree[id].circAL].circAR = evTree[0].idA;
  evTree[0].idA = evTree[id].circAR;
  evTree[id].circAL = evTree[id].circAR = id;
  for (idd = evTree[id].circBL; idd != id; idd = evTree[idd].circBL) {
    evTree[evTree[idd].circAL].circAR = evTree[idd].circAR;
    evTree[evTree[idd].circAR].circAL = evTree[idd].circAL;
    DeleteEvent (idd);
    evTree[idd].circAR = evTree[0].idA;
    evTree[0].idA = idd;
  }
  evTree[id].circBL = evTree[id].circBR = id;
}

void DeleteEvent (int id)
{
  int idp, idq, idr;

  idr = evTree[id].right;
  if (idr < 0) idq = evTree[id].left;
  else {
    if (evTree[id].left < 0) idq = idr;
    else {
      if (evTree[idr].left < 0) idq = idr;
      else {
        idq = evTree[idr].left;
        while (evTree[idq].left >= 0) {
          idr = idq;
          idq = evTree[idr].left;
        }
        evTree[idr].left = evTree[idq].right;
        if (evTree[idq].right >= 0) evTree[evTree[idq].right].up = idr;
        evTree[idq].right = evTree[id].right;
        evTree[evTree[id].right].up = idq;
      }
      evTree[evTree[id].left].up = idq;
      evTree[idq].left = evTree[id].left;
    }
  }
  idp = evTree[id].up;
  if (idq >= 0) evTree[idq].up = idp;
  if (evTree[idp].right != id) evTree[idp].left = idq;
  else evTree[idp].right = idq;
}

void InitEventList ()
{
  int id;

  evTree[0].left = evTree[0].right = -1;
  evTree[0].idA = nMol + 1;
  for (id = evTree[0].idA; id < poolSize - 1; id ++)
     evTree[id].circAR = id + 1;
  evTree[poolSize - 1].circAR = -1;
  for (id = 1; id < nMol + 1; id ++) {
    evTree[id].circAL = evTree[id].circBL = id;
    evTree[id].circAR = evTree[id].circBR = id;
  }
}


void GridAverage (int opCode)
{
  VecR invWid, rs, va;
  VecI cc;
  real pSum;
  int c, hSize, j, n;

  hSize = VProd (sizeHistGrid);
  if (opCode == 0) {
    for (j = 0; j < NHIST; j ++) {
      for (n = 0; n < hSize; n ++) histGrid[j][n] = 0.;
    }
  } else if (opCode == 1) {
    VDiv (invWid, sizeHistGrid, region);
    DO_MOL {
      VSAdd (rs, mol[n].r, 0.5, region);
      VMul (cc, rs, invWid);
      c = VLinear (cc, sizeHistGrid);
      ++ histGrid[0][c];
      histGrid[1][c] += VLenSq (mol[n].rv);
      histGrid[2][c] += mol[n].rv.x;
      histGrid[3][c] += mol[n].rv.y;
    }
  } else if (opCode == 2) {
    pSum = 0.;
    for (n = 0; n < hSize; n ++) {
      if (histGrid[0][n] > 0.) {
        for (j = 1; j < NHIST; j ++)
           histGrid[j][n] /= histGrid[0][n];
        VSet (va, histGrid[2][n], histGrid[3][n]);
        histGrid[1][n] = (histGrid[1][n] - VLenSq (va)) / NDIM;
        pSum += histGrid[0][n];
      }
    }
    pSum /= hSize;
    for (n = 0; n < hSize; n ++) histGrid[0][n] /= pSum;
  }
}


#define SCALE_FAC  32767.

void PutGridAverage ()
{
  real histMax[NHIST], w;
  int blockSize, fOk, hSize, j, n;
  short *hI;
  FILE *fp;

  hSize = VProd (sizeHistGrid);
  for (j = 0; j < NHIST; j ++) {
    histMax[j] = 0.;
    for (n = 0; n < hSize; n ++) {
      w = fabs (histGrid[j][n]);
      histMax[j] = Max (histMax[j], w);
    }
    if (histMax[j] == 0.) histMax[j] = 1.;
  }
  fOk = 1;
  blockSize = (NHIST + 1) * sizeof (real) + sizeof (VecR) +
     (NDIM + 3) * sizeof (int) + NHIST * hSize * sizeof (short);
  if ((fp = fopen (fileName[FL_SNAP], "a")) != 0) {
    WriteF (blockSize);
    WriteF (histMax);
    WriteF (region);
    WriteF (runId);
    WriteF (sizeHistGrid);
    WriteF (snapNumber);
    WriteF (timeNow);
    AllocMem (hI, hSize, short);
    for (j = 0; j < NHIST; j ++) {
      for (n = 0; n < hSize; n ++)
         hI[n] = SCALE_FAC * histGrid[j][n] / histMax[j];
      WriteFN (hI, hSize);
    }
    free (hI);
    if (ferror (fp)) fOk = 0;
    fclose (fp);
  } else fOk = 0;
  if (! fOk) ErrExit (ERR_SNAP_WRITE);
}

void SetupFiles ()
{
  strcpy (fileName[FL_SNAP], fileNameR[FL_SNAP]);
  fileName[FL_SNAP][0] = progId[0];
  fileName[FL_SNAP][1] = progId[1];
  fileName[FL_SNAP][2] = runId / 10 + CHAR_ZERO;
  fileName[FL_SNAP][3] = runId % 10 + CHAR_ZERO;
}


void EvalProfile ()
{
  int k, n;

  for (n = 0; n < sizeHistGrid.y; n ++) {
    profileD[n] = 0.;
    profileT[n] = 0.;
  }
  for (n = 0; n < VProd (sizeHistGrid); n ++) {
    k = n / sizeHistGrid.x;
    profileD[k] += histGrid[0][n];
    profileT[k] += histGrid[1][n];
  }
  for (n = 0; n < sizeHistGrid.y; n ++) {
    profileD[n] *= 1. / sizeHistGrid.x;
    profileT[n] *= 0.5 / sizeHistGrid.x;
  }
}

void PrintProfile (FILE *fp)
{
  int n;

  fprintf (fp, "%.3f\n", timeNow);
  fprintf (fp, "T: ");
  for (n = 0; n < sizeHistGrid.y; n ++) fprintf (fp, "%.2f ", profileT[n]);
  fprintf (fp, "\n");
  fprintf (fp, "D: ");
  for (n = 0; n < sizeHistGrid.y; n ++) fprintf (fp, "%.2f ", profileD[n]);
  fprintf (fp, "\n");
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

