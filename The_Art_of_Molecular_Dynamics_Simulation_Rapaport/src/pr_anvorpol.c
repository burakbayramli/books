
/* [[pr_anvorpol - Voronoi polyhedra analysis]] */


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


#define READ_CONFIG  1

#include "in_mddefs.h"

#define MAX_EDGE   200
#define MAX_FACE   50
#define MAX_FLIST  500
#define MAX_ITEM   50
#define MAX_VERT   200

typedef struct {
  VecR r;
  int inCell;
} Mol;
typedef struct {
  int f[2], v[2], stat;
} Edge;
typedef struct {
  real dist;
  int fPtr, stat, vFar;
} Face;
typedef struct {
  int e, link, v;
} Flist;
typedef struct {
  VecR pos;
  real distSq;
  int e[3], stat;
} Vert;

Mol *mol;
Edge *edge;
Face *face;
Flist *flist;
Vert *vert;
VecR fParamV, region;
VecI cells;
real *distSq, cellRatio, eulerSum, fParamS, fracPolyVol, rangeLim,
   regionVol, timeNow, vDistSqMax;
Prop polyGeom[4], polyArea, polyVol;
int *cellList, *eCut, *eDel, *eNew, *fCut, *fDel, *siteSeq,
   *testSites, *vDel, blockNum, blockSize, curSite, eLast, eLastP, fLast,
   fListLast, nCell, neCut, neDel, neNew, nfCut, nfDel, nMol, nTestSites,
   nvDel, runId, siteA, siteB, stepCount, vLast;
FILE *fp;
#if ! READ_CONFIG
 int randSeed;
#endif

#if ! READ_CONFIG

void InitCoords ()
{
  real randTab[100];
  int i, n, k;

  for (i = 0; i < 100; i ++) randTab[i] = RandR ();
  DO_MOL {
    for (k = 0; k < NDIM; k ++) {
      i = (int) (100. * RandR ());
      VComp (mol[n].r, k) = (randTab[i] - 0.5) * VComp (region, k);
      randTab[i] = RandR ();
    }
  }
}


#define IADD   453806245
#define IMUL   314159269
#define MASK   2147483647
#define SCALE  0.4656612873e-9

int randSeedP = 17;

void InitRand (int randSeedI)
{
  struct timeval tv;

  if (randSeedI != 0) randSeedP = randSeedI;
  else {
    gettimeofday (&tv, 0);
    randSeedP = tv.tv_usec;
  }
}

real RandR ()
{
  randSeedP = (randSeedP * IMUL + IADD) & MASK;
  return (randSeedP * SCALE);
}

#if NDIM == 2

void VRand (VecR *p)
{
  real s;

  s = 2. * M_PI * RandR ();
  p->x = cos (s);
  p->y = sin (s);
}

#elif NDIM == 3

void VRand (VecR *p)
{
  real s, x, y;

  s = 2.;
  while (s > 1.) {
    x = 2. * RandR () - 1.;
    y = 2. * RandR () - 1.;
    s = Sqr (x) + Sqr (y);
  }
  p->z = 1. - 2. * s;
  s = 2. * sqrt (1. - s);
  p->x = s * x;
  p->y = s * y;
}

#endif


#endif

int main (int argc, char **argv)
{
  int n, na;

#if ! READ_CONFIG
  randSeed = 17;
  nMol = 4000;
  printf ("nMol %d randSeed: %d\n", nMol, randSeed);
  VSetAll (region, 10.);
  cellRatio = 0.5;
  SetCellSize ();
  AllocArrays ();
  InitRand (randSeed);
  InitCoords ();
  if (1) {
#else
  runId = atoi (argv[1]);
  cellRatio = 0.5;
  SetupFiles ();
  blockNum = -1;
  while (GetConfig ()) {
    printf ("id: %d  steps: %d\n", runId, stepCount);
#endif
    regionVol = VProd (region);
    SubdivCells ();
    rangeLim = region.x;
    PropZero (polyArea);
    PropZero (polyVol);
    for (n = 0; n < 4; n ++) PropZero (polyGeom[n]);
    for (na = 0; na < nMol; na ++) {
      FindTestSites (na);
      AnalVorPoly ();
      PropAccum (polyArea);
      PropAccum (polyVol);
      for (n = 0; n < 4; n ++) PropAccum (polyGeom[n]);
    }
    fracPolyVol = polyVol.sum / regionVol;
    PropAvg (polyArea, nMol);
    PropAvg (polyVol, nMol);
    for (n = 0; n < 4; n ++) PropAvg (polyGeom[n], nMol);
    polyArea.sum /= pow (regionVol, 2./3.);
    polyArea.sum2 /= pow (regionVol, 2./3.);
    polyVol.sum /= regionVol;
    polyVol.sum2 /= regionVol;
    eulerSum = polyGeom[0].sum + polyGeom[2].sum - polyGeom[1].sum;
    printf ("area: %.3f %.3f\n", PropEst (polyArea));
    printf ("vol: %.3f %.3f\n", PropEst (polyVol));
    for (n = 0; n < 4; n ++) {
      printf ("geom: %.3f %.3f\n", PropEst (polyGeom[n]));
    }
    printf ("euler: %.3f\n", eulerSum);
    printf ("vol frac: %.4f\n", fracPolyVol);
    printf ("\n");
    fflush (stdout);
  }
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (testSites, nMol, int);
  AllocMem (distSq, nMol, real);
  AllocMem (siteSeq, nMol, int);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (edge, MAX_EDGE, Edge);
  AllocMem (vert, MAX_VERT, Vert);
  AllocMem (face, MAX_FACE, Face);
  AllocMem (flist, MAX_FLIST, Flist);
  AllocMem (eCut, MAX_ITEM, int);
  AllocMem (eDel, MAX_ITEM, int);
  AllocMem (eNew, MAX_ITEM, int);
  AllocMem (vDel, MAX_ITEM, int);
  AllocMem (fCut, MAX_ITEM, int);
  AllocMem (fDel, MAX_ITEM, int);
}

void AnalVorPoly ()
{
  int nf;

  Sort (distSq, siteSeq, nTestSites);
  InitVorPoly ();
  for (curSite = 0; curSite < nTestSites; curSite ++) {
    if (distSq[siteSeq[curSite]] >= 4. * vDistSqMax) break;
    siteB = testSites[siteSeq[curSite]];
    nvDel = 0;
    neNew = 0;
    neDel = 0;
    neCut = 0;
    nfDel = 0;
    nfCut = 0;
    BisectPlane ();
    if (nvDel > 0) ProcDelVerts ();
    if (neCut > 0) ProcCutEdges ();
    if (nfCut > 0) ProcCutFaces ();
    if (neNew > 0) ProcNewVerts ();
    if (nfCut > 0) ProcNewFace ();
    RemoveOld ();
    if (nfCut > 0) FindDistVerts ();
  }
  for (nf = 0; nf < 4; nf ++)
     if (face[nf].stat != 0) ErrExit (ERR_SUBDIV_UNFIN);
  PolyGeometry ();
  PolySize ();
}

void SetCellSize ()
{
  VSCopy (cells, cellRatio, region);
  nCell = VProd (cells);
}

void SubdivCells ()
{
  VecR invWid, rs;
  VecI cc;
  int n, c;

  VDiv (invWid, cells, region);
  for (n = nMol; n < nMol + VProd (cells); n ++) cellList[n] = -1;
  DO_MOL {
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nMol;
    mol[n].inCell = c - nMol;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
}

void FindTestSites (int na)
{
  VecR dr;
  VecI cn;
  int c, cx, cy, cz, i, ofx, ofy, ofz;

  cx = mol[na].inCell % cells.x;
  cy = (mol[na].inCell / cells.x) % cells.y;
  cz = mol[na].inCell / (cells.x * cells.y);
  nTestSites = 0;
  for (ofz = -1; ofz <= 1; ofz ++) {
    cn.z = (cz + ofz + cells.z) % cells.z;
    for (ofy = -1; ofy <= 1; ofy ++) {
      cn.y = (cy + ofy + cells.y) % cells.y;
      for (ofx = -1; ofx <= 1; ofx ++) {
        cn.x = (cx + ofx + cells.x) % cells.x;
        c = VLinear (cn, cells) + nMol;
        DO_CELL (i, c) {
          VSub (dr, mol[na].r, mol[i].r);
          VWrapAll (dr);
          testSites[nTestSites] = i;
          distSq[nTestSites] = VLenSq (dr);
          ++ nTestSites;
        }
      }
    }
  }
}

void InitVorPoly ()
{
  VecR w, vPosI[] =
     { {-1.,-1.,-1.}, {1.,-1.,-1.}, {0.,2.,-1.}, {0.,0.,3.} };
  real r2, r6;
  int m, n, ne, nf, nv, s,
     vValI[] = {0,2,5,0,1,4,1,2,3,3,4,5},
     eFacesI[] = {0,3,0,1,0,2,1,2,1,3,2,3},
     eVertsI[] = {0,1,1,2,0,2,2,3,1,3,0,3},
     eI[] = {0,1,2,4,3,1,2,3,5,5,4,0},
     vI[] = {0,1,2,1,3,2,0,2,3,0,3,1};

  r2 = sqrt (2.) * rangeLim;
  r6 = sqrt (6.) * rangeLim;
  siteA = testSites[siteSeq[0]];
  eLast = 5;
  fLast = 3;
  vLast = 3;
  m = 0;
  for (nv = 0; nv <= vLast; nv ++) {
    vert[nv].pos = mol[siteA].r;
    VSet (w, r6 / 3., r2 / 3., rangeLim / 3.);
    VMul (w, w, vPosI[nv]);
    VVAdd (vert[nv].pos, w);
    vert[nv].distSq = Sqr (rangeLim);
    vert[nv].stat = 2;
    for (n = 0; n < 3; n ++) {
      vert[nv].e[n] = vValI[m];
      ++ m;
    }
  }
  vDistSqMax = vert[0].distSq;
  for (ne = 0; ne <= eLast; ne ++) {
    edge[ne].v[0] = eVertsI[2 * ne];
    edge[ne].f[0] = eFacesI[2 * ne];
    edge[ne].v[1] = eVertsI[2 * ne + 1];
    edge[ne].f[1] = eFacesI[2 * ne + 1];
    edge[ne].stat = 3;
  }
  for (s = 0; s < MAX_FLIST - 1; s ++) flist[s].link = s + 1;
  s = 0;
  for (nf = 0; nf <= fLast; nf ++) {
    face[nf].vFar = vI[s];
    face[nf].stat = 3;
    face[nf].fPtr = s;
    for (n = 0; n < 3; n ++) {
      flist[s].v = vI[s];
      flist[s].e = eI[s];
      ++ s;
    }
    flist[s - 1].link = face[nf].fPtr;
  }
  fListLast = s - 1;
}

void BisectPlane ()
{
  VecR dr, shift;
  real d1, d2, d3;
  int nv;

  d1 = 0.;
  fParamS = 0.;
  VSub (fParamV, mol[siteB].r, mol[siteA].r);
  VZero (shift);
  VShiftAll (fParamV);
  VVAdd (fParamV, shift);
  d1 = VDot (fParamV, mol[siteA].r);
  VAdd (dr, mol[siteB].r, shift);
  fParamS = 0.5 * (VLenSq (dr) - VLenSq (mol[siteA].r));
  for (nv = 0; nv <= vLast; nv ++) {
    if (vert[nv].stat != 0) {
      d2 = VDot (fParamV, vert[nv].pos);
      if (d1 != d2) {
        d3 = (fParamS - d1) / (d2 - d1);
        if (d3 > 0. && d3 < 1.) {
          vDel[nvDel] = nv;
          ++ nvDel;
          vert[nv].stat = 1;
        }
      }
    }
  }
}

void ProcDelVerts ()
{
  int e, m, n, nv;

  for (nv = 0; nv < nvDel; nv ++) {
    for (m = 0; m < 3; m ++) {
      e = vert[vDel[nv]].e[m];
      -- edge[e].stat;
      if (edge[e].stat == 2) {
        eCut[neCut] = e;
        ++ neCut;
      } else {
        eDel[neDel] = e;
        ++ neDel;
      }
      for (n = 0; n < 2; n ++) {
        if (face[edge[e].f[n]].stat == 3) {
          fCut[nfCut] = edge[e].f[n];
          ++ nfCut;
          face[edge[e].f[n]].stat = 2;
        }
      }
    }
  }
}

void ProcCutEdges ()
{
  VecR dr;
  real d, dt1, dt2;
  int nd, ne, vt1, vt2;

  for (ne = 0; ne < neCut; ne ++) {
    if (edge[eCut[ne]].stat == 2) {
      edge[eCut[ne]].stat = 3;
      vt1 = edge[eCut[ne]].v[0];
      vt2 = edge[eCut[ne]].v[1];
      dt1 = VDot (fParamV, vert[vt1].pos);
      dt2 = VDot (fParamV, vert[vt2].pos);
      if (vert[vt1].stat == 1) nd = 0;
      else if (vert[vt2].stat == 1) nd = 1;
      ++ vLast;
      vert[vLast].stat = 2;
      vert[vLast].distSq = 0.;
      d = (fParamS - dt1) / (dt2 - dt1);
      VInterp (vert[vLast].pos, d, vert[vt2].pos, vert[vt1].pos);
      VSub (dr, vert[vLast].pos, mol[siteA].r);
      vert[vLast].distSq = VLenSq (dr);
      edge[eCut[ne]].v[nd] = vLast;
      vert[vLast].e[0] = eCut[ne];
      vert[vLast].e[1] = 0;
      vert[vLast].e[2] = 0;
    }
  }
}

void ProcCutFaces ()
{
  int faceGone, nf, s, s1, s2, s3, s4, v1, v2, vDelCount;

  eLastP = eLast;
  ++ fLast;
  for (nf = 0; nf < nfCut; nf ++) {
    s = face[fCut[nf]].fPtr;
    faceGone = 0;
    while (vert[flist[s].v].stat != 2 && ! faceGone) {
      s = flist[s].link;
      if (s == face[fCut[nf]].fPtr) faceGone = 1;
    }
    if (faceGone) {
      fDel[nfDel] = fCut[nf];
      face[fCut[nf]].stat = 1;
      ++ nfDel;
    } else {
      face[fCut[nf]].stat = 3;
      face[fCut[nf]].fPtr = s;
      for (s1 = s, s2 = flist[s1].link; vert[flist[s2].v].stat == 2;
         s2 = flist[s1].link) s1 = s2;
      vDelCount = 1;
      for (s3 = s2, s4 = flist[s3].link; vert[flist[s4].v].stat != 2;
         s4 = flist[s3].link) {
        ++ vDelCount;
        s3 = s4;
      }
      v1 = edge[flist[s1].e].v[0] + edge[flist[s1].e].v[1] - flist[s1].v;
      v2 = edge[flist[s3].e].v[0] + edge[flist[s3].e].v[1] - flist[s4].v;
      ++ eLast;
      flist[s3].v = v2;
      if (vDelCount == 1) {
        ++ fListLast;
        s = fListLast;
        flist[s1].link = s;
        flist[s].link = s2;
        flist[s].v = v1;
        flist[s].e = eLast;
      } else {
        flist[s2].v = v1;
        flist[s2].e = eLast;
        if (vDelCount > 2) flist[s2].link = s3;
      }
      edge[eLast].v[0] = v1;
      edge[eLast].v[1] = v2;
      edge[eLast].f[0] = fCut[nf];
      edge[eLast].f[1] = fLast;
      edge[eLast].stat = 2;
      eNew[neNew] = eLast;
      ++ neNew;
    }
  }
}

void ProcNewVerts ()
{
  int ne, v;

  for (ne = 0; ne < neNew; ne ++) {
    if (eNew[ne] > eLastP) {
      v = edge[eNew[ne]].v[0];
      if (vert[v].e[1] == 0) vert[v].e[1] = eNew[ne];
      else vert[v].e[2] = eNew[ne];
      v = edge[eNew[ne]].v[1];
      if (vert[v].e[1] == 0) vert[v].e[1] = eNew[ne];
      else vert[v].e[2] = eNew[ne];
    }
  }
}

void ProcNewFace ()
{
  int e, n, ne, v;

  for (n = 0; n < neNew; n ++) {
    ++ fListLast;
    if (n == 0) {
      e = eNew[0];
      face[fLast].fPtr = fListLast;
      v = edge[e].v[0];
    } else {
      ne = 1;
      for (e = eNew[ne]; edge[e].v[0] != v && edge[e].v[1] != v ||
         edge[e].stat == 3; e = eNew[ne]) ++ ne;
    }
    flist[fListLast].v = v;
    v = edge[e].v[0] + edge[e].v[1] - v;
    flist[fListLast].e = e;
    edge[e].stat = 3;
  }
  face[fLast].stat = 3;
  flist[fListLast].link = face[fLast].fPtr;
  face[fLast].dist = 0.5 * sqrt (distSq[siteSeq[curSite]]);
}

void RemoveOld ()
{
  int n;

  for (n = 0; n < nvDel; n ++) vert[vDel[n]].stat = 0;
  for (n = 0; n < neDel; n ++) {
    if (edge[eDel[n]].stat == 1) edge[eDel[n]].stat = 0;
  }
  for (n = 0; n < nfDel; n ++) face[fDel[n]].stat = 0;
}

void FindDistVerts ()
{
  real dd;
  int nf, s;

  fCut[nfCut] = fLast;
  for (nf = 0; nf < nfCut + 1; nf ++) {
    if (face[fCut[nf]].stat != 0) {
      s = face[fCut[nf]].fPtr;
      dd = vert[flist[s].v].distSq;
      face[fCut[nf]].vFar = flist[s].v;
      for (s = flist[s].link; s != face[fCut[nf]].fPtr;
         s = flist[s].link) {
        if (vert[flist[s].v].distSq > dd) {
          dd = vert[flist[s].v].distSq;
          face[fCut[nf]].vFar = flist[s].v;
        }
      }
    }
  }
  vDistSqMax = 0.;
  for (nf = 0; nf <= fLast; nf ++) {
    if (face[nf].stat != 0 &&
       vDistSqMax < vert[face[nf].vFar].distSq)
       vDistSqMax = vert[face[nf].vFar].distSq;
  }
}

void PolyGeometry ()
{
  int n, ne, nf, nv, s;

  for (n = 0; n < 4; n ++) polyGeom[n].val = 0.;
  for (nv = 0; nv <= vLast; nv ++) {
    if (vert[nv].stat != 0) ++ polyGeom[0].val;
  }
  for (ne = 0; ne <= eLast; ne ++) {
    if (edge[ne].stat != 0) ++ polyGeom[1].val;
  }
  for (nf = 0; nf <= fLast; nf ++) {
    if (face[nf].stat != 0) {
      ++ polyGeom[2].val;
      ++ polyGeom[3].val;
      for (s = flist[face[nf].fPtr].link;
         s != face[nf].fPtr; s = flist[s].link) ++ polyGeom[3].val;
    }
  }
  polyGeom[3].val /= polyGeom[2].val;
}

void PolySize ()
{
  VecR ca, d1, d2, d3;
  real a;
  int nf, s, v1, v2;

  polyArea.val = 0.;
  polyVol.val = 0.;
  for (nf = 0; nf <= fLast; nf ++) {
    if (face[nf].stat != 0) {
      s = face[nf].fPtr;
      v1 = flist[s].v;
      s = flist[s].link;
      v2 = flist[s].v;
      VSub (d1, vert[v2].pos, vert[v1].pos);
      VZero (ca);
      for (s = flist[s].link; s != face[nf].fPtr;
         s = flist[s].link) {
        v2 = flist[s].v;
        VSub (d2, vert[v2].pos, vert[v1].pos);
        VCross (d3, d1, d2);
        VVAdd (ca, d3);
        d1 = d2;
      }
      a = VLen (ca);
      polyArea.val += a / 2.;
      polyVol.val += face[nf].dist * a / 6.;
    }
  }
}


void Sort (real *a, int *seq, int n)
{
  real q;
  int i, ir, ixt, j, k;

  for (j = 0; j < n; j ++) seq[j] = j;
  if (n > 1) {
    k = n / 2;
    ir = n - 1;
    while (1) {
      if (k > 0) {
        -- k;
        ixt = seq[k];
        q = a[ixt];
      } else {
        ixt = seq[ir];
        q = a[ixt];
        seq[ir] = seq[0];
        -- ir;
        if (ir == 0) {
          seq[0] = ixt;
          break;
        }
      }
      i = k;
      j = 2 * k + 1;
      while (j <= ir) {
        if (j < ir && a[seq[j]] < a[seq[j + 1]]) ++ j;
        if (q < a[seq[j]]) {
          seq[i] = seq[j];
          i = j;
          j = 2 * j + 1;
        } else j = ir + 1;
      }
      seq[i] = ixt;
    }
  }
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

