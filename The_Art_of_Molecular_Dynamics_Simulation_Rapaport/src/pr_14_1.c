
/* [[pr_14_1 - hard spheres, RDF]] */


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
  VecR r, rv;
  real time;
  VecI inCell;
} Mol;

Mol *mol;
EvTree *evTree;
VecR region, vSum;
VecI cellRange[2], cells, initUcell;
real collCount, crossCount, density, intervalSum, nextSumTime,
   kinEnVal, temperature, timeNow, velMag;
int *cellList, eventCount, eventMult, evIdA, evIdB, limitEventCount, moreCycles,
   nMol, poolSize;
real *histRdf, intervalRdf, nextRdfTime, rangeRdf;
int countRdf, limitRdf, sizeHistRdf;

NameList nameList[] = {
  NameR (density),
  NameI (eventMult),
  NameI (initUcell),
  NameR (intervalRdf),
  NameR (intervalSum),
  NameI (limitEventCount),
  NameI (limitRdf),
  NameR (rangeRdf),
  NameI (sizeHistRdf),
  NameR (temperature),
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
  AllocArrays ();
  InitCoords ();
  InitVels ();
  timeNow = nextSumTime = 0.;
  collCount = crossCount = 0.;
  StartRun ();
  ScheduleEvent (0, MOL_LIMIT + 6, nextSumTime);
  nextRdfTime = 0.;
  ScheduleEvent (0, MOL_LIMIT + 7, nextRdfTime);
  countRdf = 0;
}

void SingleEvent ()
{
  real vvSum;
  int n;

  NextEvent ();
  if (evIdB < MOL_LIMIT) {
    ProcessCollision ();
    ++ collCount;
  } else if (evIdB >= MOL_LIMIT + 100) {
    ProcessCellCrossing ();
    ++ crossCount;
  } else if (evIdB == MOL_LIMIT + 7) {
    UpdateSystem ();
    EvalRdf ();
    nextRdfTime += intervalRdf;
    ScheduleEvent (0, MOL_LIMIT + 7, nextRdfTime);
  } else if (evIdB == MOL_LIMIT + 6) {
    UpdateSystem ();
    nextSumTime += intervalSum;
    ScheduleEvent (0, MOL_LIMIT + 6, nextSumTime);
    VZero (vSum);
    vvSum = 0.;
    DO_MOL {
      VVAdd (vSum, mol[n].rv);
      vvSum += VLenSq (mol[n].rv);
    }
    kinEnVal = vvSum * 0.5 / nMol;
    PrintSummary (stdout);
  }
}

void SetParams ()
{
  VSCopy (region, 1. / pow (density / 4., 1./3.), initUcell);
  nMol = 4 * VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  poolSize = eventMult * nMol;
  VCopy (cells, region);
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (evTree, poolSize, EvTree);
  AllocMem (histRdf, sizeHistRdf, real);
}

void PrintSummary (FILE *fp)
{
  fprintf (fp, "%.2f %.10g %.10g %.3f",
     timeNow, collCount, crossCount, VCSum (vSum) / nMol);
  fprintf (fp, " %.3f", kinEnVal);
  fprintf (fp, "\n");
  fflush (fp);
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


void ProcessCollision ()
{
  VecR dr, dv;
  real fac;

  UpdateMol (evIdA);
  UpdateMol (evIdB);
  VSetAll (cellRange[0], -1);
  VSetAll (cellRange[1], 1);
  VSub (dr, mol[evIdA].r, mol[evIdB].r);
  VWrapAll (dr);
  VSub (dv, mol[evIdA].rv, mol[evIdB].rv);
  fac = - VDot (dr, dv) / VLenSq (dr);
  VVSAdd (mol[evIdA].rv, fac, dr);
  VVSAdd (mol[evIdB].rv, - fac, dr);
  PredictEvent (evIdA, -1);
  PredictEvent (evIdB, evIdA);
}

#define VWrapEv(t)                                          \
   if (mol[evIdA].rv.t > 0.) {                              \
     cellRange[0].t = 1;                                    \
     ++ mol[evIdA].inCell.t;                                \
     if (mol[evIdA].inCell.t == cells.t) {                  \
       mol[evIdA].inCell.t = 0;                             \
       mol[evIdA].r.t = -0.5 * region.t;                    \
     }                                                      \
   } else {                                                 \
     cellRange[1].t = -1;                                   \
     -- mol[evIdA].inCell.t;                                \
     if (mol[evIdA].inCell.t == -1) {                       \
       mol[evIdA].inCell.t = cells.t - 1;                   \
       mol[evIdA].r.t = 0.5 * region.t;                     \
     }                                                      \
   }

void ProcessCellCrossing ()
{
  int n;

  UpdateMol (evIdA);
  n = VLinear (mol[evIdA].inCell, cells) + nMol;
  while (cellList[n] != evIdA) n = cellList[n];
  cellList[n] = cellList[evIdA];
  VSetAll (cellRange[0], -1);
  VSetAll (cellRange[1], 1);
  switch (evIdB - MOL_LIMIT - 100) {
    case 0:
      VWrapEv (x);
      break;
    case 1:
      VWrapEv (y);
      break;
    case 2:
      VWrapEv (z);
      break;
  }
  PredictEvent (evIdA, evIdB);
  n = VLinear (mol[evIdA].inCell, cells) + nMol;
  cellList[evIdA] = cellList[n];
  cellList[n] = evIdA;
}

#define WhenCross(t)                                        \
   tm.t = (mol[na].rv.t != 0.) ? w.t / mol[na].rv.t : 1e12;

#define VWrapEvC(t)                                         \
   m2v.t = mol[na].inCell.t + m1v.t;                        \
   shift.t = 0.;                                            \
   if (m2v.t == -1) {                                       \
     m2v.t = cells.t - 1;                                   \
     shift.t = - region.t;                                  \
   } else if (m2v.t == cells.t) {                           \
     m2v.t = 0;                                             \
     shift.t = region.t;                                    \
   }

void PredictEvent (int na, int nb)
{
  VecR dr, dv, rs, shift, tm, w;
  VecI m1v, m2v;
  real b, d, t, tInt, vv;
  int dir, evCode, m1x, m1y, m1z, n;

  VCopy (w, mol[na].inCell);
  if (mol[na].rv.x > 0.) ++ w.x;
  if (mol[na].rv.y > 0.) ++ w.y;
  if (mol[na].rv.z > 0.) ++ w.z;
  VMul (w, w, region);
  VDiv (w, w, cells);
  VSAdd (rs, mol[na].r, 0.5, region);
  VVSub (w, rs);
  WhenCross (x);
  WhenCross (y);
  WhenCross (z);
  if (tm.y < tm.z) dir = (tm.x < tm.y) ? 0 : 1;
  else dir = (tm.x < tm.z) ? 0 : 2;
  evCode = 100 + dir;
  ScheduleEvent (na, MOL_LIMIT + evCode, timeNow + VComp (tm, dir));
  for (m1z = cellRange[0].z; m1z <= cellRange[1].z; m1z ++) {
    m1v.z = m1z;
    VWrapEvC (z);
    for (m1y = cellRange[0].y; m1y <= cellRange[1].y; m1y ++) {
      m1v.y = m1y;
      VWrapEvC (y);
      for (m1x = cellRange[0].x; m1x <= cellRange[1].x; m1x ++) {
        m1v.x = m1x;
        VWrapEvC (x);
        n = VLinear (m2v, cells) + nMol;
        for (n = cellList[n]; n >= 0; n = cellList[n]) {
          if (n != na && n != nb && (nb >= -1 || n < na)) {
            tInt = timeNow - mol[n].time;
            VSub (dr, mol[na].r, mol[n].r);
            VVSAdd (dr, - tInt, mol[n].rv);
            VVSub (dr, shift);
            VSub (dv, mol[na].rv, mol[n].rv);
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


void EvalRdf ()
{
  VecR dr;
  real deltaR, normFac, rr;
  int j1, j2, n;

  if (countRdf == 0) {
    for (n = 0; n < sizeHistRdf; n ++) histRdf[n] = 0.;
  }
  deltaR = rangeRdf / sizeHistRdf;
  for (j1 = 0; j1 < nMol - 1; j1 ++) {
    for (j2 = j1 + 1; j2 < nMol; j2 ++) {
      VSub (dr, mol[j1].r, mol[j2].r);
      VWrapAll (dr);
      rr = VLenSq (dr);
      if (rr < Sqr (rangeRdf)) {
        n = sqrt (rr) / deltaR;
        ++ histRdf[n];
      }
    }
  }
  ++ countRdf;
  if (countRdf == limitRdf) {
    normFac = VProd (region) / (2. * M_PI * Cube (deltaR) *
       Sqr (nMol) * countRdf);
    for (n = 0; n < sizeHistRdf; n ++)
       histRdf[n] *= normFac / Sqr (n - 0.5);
    PrintRdf (stdout);
    countRdf = 0;
  }
}

void PrintRdf (FILE *fp)
{
  real rb;
  int n;

  fprintf (fp, "rdf\n");
  for (n = 0; n < sizeHistRdf; n ++) {
    rb = (n + 0.5) * rangeRdf / sizeHistRdf;
    fprintf (fp, "%8.4f %8.4f\n", rb, histRdf[n]);
  }
  fflush (fp);
}


#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

