
/* [[pr_08_5 - tetrahedral molecules]] */


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
  VecR wv, wa;
  RMat rMatT;
} Mol;
typedef struct {
  VecR f, r;
} Site;
typedef struct {
  VecR r;
} MSite;

Mol *mol;
Site *site;
MSite *mSite;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
VecR mInert;
real siteSep, farSiteDist;
int sitesMol, nSite;
int stepAdjustTemp;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (nebrTabFac),
  NameR (rNebrShell),
  NameI (stepAdjustTemp),
  NameI (stepAvg),
  NameI (stepEquil),
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
  GenSiteCoords ();
  if (nebrNow) {
    nebrNow = 0;
    dispHi = 0.;
    BuildNebrList ();
  }
  ComputeSiteForces ();
  ComputeWallForces ();
  ComputeTorqs ();
  ApplyThermostat ();
  LeapfrogStep (2);
  EvalProps ();
  if (stepCount % stepAdjustTemp == 0 || stepCount < stepEquil &&
     stepCount % 100 == 0) AdjustTemp ();
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
  DefineMol ();
  stepCount = 0;
  InitCoordsWalls (farSiteDist);
  InitVels ();
  InitAccels ();
  InitAngCoords ();
  InitAngVels ();
  InitAngAccels ();
  AccumProps (0);
  nebrNow = 1;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  sitesMol = 4;
  siteSep = 0.8;
  farSiteDist = siteSep / (2. * sqrt (2./3.));
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nMol = VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / (rCut + rNebrShell), region);
  nSite = nMol * sitesMol;
  nebrTabMax = nebrTabFac * nSite;
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (site, nSite, Site);
  AllocMem (mSite, sitesMol, MSite);
  AllocMem (cellList, VProd (cells) + nSite, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
}


#define Mn(a, i, j)  MAT (a, n, i, j)

void MulMat (real *a, real *b, real *c, int n)
{
  int i, j, k;

  for (i = 0; i < n; i ++) {
    for (j = 0; j < n; j ++) {
      Mn (a, i, j) = 0.;
      for (k = 0; k < n; k ++) Mn (a, i, j) += Mn (b, i, k) * Mn (c, k, j);
    }
  }
}

void BuildStepRmatT (RMat *mp, VecR *a)
{
  RMat m1, m2;
  real c[3], s[3], ak, c0c2, c0s2, s0c2, s0s2, t;
  int k;

  for (k = 0; k < 3; k ++) {
    ak = VComp (*a, k);
    t = 0.25 * Sqr (ak);
    c[k] = (1. - t) / (1. + t);
    s[k] = ak / (1. + t);
  }
  c0c2 = c[0] * c[2];
  c0s2 = c[0] * s[2];
  s0c2 = s[0] * c[2];
  s0s2 = s[0] * s[2];
  m1.u[0] = c[1] * c[2];
  m1.u[1] = s0c2 * s[1] + c0s2;
  m1.u[2] = - c0c2 * s[1] + s0s2;
  m1.u[3] = - c[1] * s[2];
  m1.u[4] = - s0s2 * s[1] + c0c2;
  m1.u[5] = c0s2 * s[1] + s0c2;
  m1.u[6] = s[1];
  m1.u[7] = - s[0] * c[1];
  m1.u[8] = c[0] * c[1];
  m2.u[0] = m1.u[0];
  m2.u[1] = - m1.u[3];
  m2.u[2] = - m1.u[6];
  m2.u[3] = s0c2 * s[1] - c0s2;
  m2.u[4] = s0s2 * s[1] + c0c2;
  m2.u[5] = - m1.u[7];
  m2.u[6] = c0c2 * s[1] + s0s2;
  m2.u[7] = c0s2 * s[1] - s0c2;
  m2.u[8] = m1.u[8];
  MulMat (mp->u, m1.u, m2.u, 3);
}


void BuildNebrList ()
{
  VecR dr, invWid, rs;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrNebr;
  int c, j1, j2, m1, m1x, m1y, m1z, m2, n, offset;

  rrNebr = Sqr (rCut + rNebrShell);
  VDiv (invWid, cells, region);
  for (n = nSite; n < nSite + VProd (cells); n ++) cellList[n] = -1;
  for (n = 0; n < nSite; n ++) {
    VSAdd (rs, site[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nSite;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
  nebrTabLen = 0;
  for (m1z = 0; m1z < cells.z; m1z ++) {
    for (m1y = 0; m1y < cells.y; m1y ++) {
      for (m1x = 0; m1x < cells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, cells) + nSite;
        for (offset = 0; offset < N_OFFSET; offset ++) {
          VAdd (m2v, m1v, vOff[offset]);
          if (m2v.x < 0 || m2v.x >= cells.x ||
              m2v.y < 0 || m2v.y >= cells.y ||
                           m2v.z >= cells.z) continue;
          m2 = VLinear (m2v, cells) + nSite;
          DO_CELL (j1, m1) {
            DO_CELL (j2, m2) {
              if ((m1 != m2 || j2 < j1) && j1 / sitesMol != j2 / sitesMol) {
                VSub (dr, site[j1].r, site[j2].r);
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


void ComputeSiteForces ()
{
  VecR dr;
  real fcVal, rr, rrCut, rri, rri3, uVal;
  int j1, j2, n;

  rrCut = Sqr (rCut);
  for (n = 0; n < nSite; n ++) VZero (site[n].f);
  uSum = 0.;
  for (n = 0; n < nebrTabLen; n ++) {
    j1 = nebrTab[2 * n];
    j2 = nebrTab[2 * n + 1];
    VSub (dr, site[j1].r, site[j2].r);
    rr = VLenSq (dr);
    if (rr < rrCut) {
      rri = 1. / rr;
      rri3 = Cube (rri);
      fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
      uVal = 4. * rri3 * (rri3 - 1.) + 1.;
      VVSAdd (site[j1].f, fcVal, dr);
      VVSAdd (site[j2].f, - fcVal, dr);
      uSum += uVal;
    }
  }
}


#define WallForce(t)                                        \
   {dr = ((site[j].r.t >= 0.) ? site[j].r.t :               \
       - site[j].r.t) - 0.5 * (region.t + rCut);            \
     if (dr > - rCut) {                                     \
       if (site[j].r.t < 0.) dr = - dr;                     \
       rri = 1. / Sqr (dr);                                 \
       rri3 = Cube (rri);                                   \
       site[j].f.t += 48. * rri3 * (rri3 - 0.5) * rri * dr; \
       uSum += 4. * rri3 * (rri3 - 1.) + 1.;                \
     }                                                      \
   }

#define NearWall(t)                                         \
   fabs (fabs (mol[n].r.t) - 0.5 * region.t) < farSiteDist + 0.5 * rCut

void ComputeWallForces ()
{
  real dr, rri, rri3;
  int j, n;

  DO_MOL {
    if (NearWall (x)) {
      for (j = n * sitesMol; j < (n + 1) * sitesMol; j ++) WallForce (x);
    }
    if (NearWall (y)) {
      for (j = n * sitesMol; j < (n + 1) * sitesMol; j ++) WallForce (y);
    }
    if (NearWall (z)) {
      for (j = n * sitesMol; j < (n + 1) * sitesMol; j ++) WallForce (z);
    }
  }
}


void GenSiteCoords ()
{
  VecR t;
  int j, n;

  DO_MOL {
    for (j = 0; j < sitesMol; j ++) {
      MVMul (t, mol[n].rMatT.u, mSite[j].r);
      VAdd (site[sitesMol * n + j].r, mol[n].r, t);
    }
  }
}

void ComputeTorqs ()
{
  VecR dr, t, torqS, waB;
  int j, n;

  DO_MOL {
    VZero (mol[n].ra);
    VZero (torqS);
    for (j = 0; j < sitesMol; j ++) {
      VVAdd (mol[n].ra, site[n * sitesMol + j].f);
      VSub (dr, site[n * sitesMol + j].r, mol[n].r);
      VCross (t, dr, site[n * sitesMol + j].f);
      VVAdd (torqS, t);
    }
    MVMulT (waB, mol[n].rMatT.u, torqS);
    VDiv (waB, waB, mInert);
    MVMul (mol[n].wa, mol[n].rMatT.u, waB);
  }
}


void DefineMol ()
{
  VecR rCm;
  int j;

  VSet (mSite[0].r, 0., 0.5 / sqrt (3.), sqrt (2.) / sqrt (3.));
  VSet (mSite[1].r, 0., 1.5 / sqrt (3.), 0.);
  VSet (mSite[2].r, 0.5, 0., 0.);
  VSet (mSite[3].r, - 0.5, 0., 0.);
  for (j = 0; j < sitesMol; j ++) VScale (mSite[j].r, siteSep);
  VZero (rCm);
  for (j = 0; j < sitesMol; j ++) VVAdd (rCm, mSite[j].r);
  for (j = 0; j < sitesMol; j ++) VVSAdd (mSite[j].r, -1. / sitesMol, rCm);
  VZero (mInert);
  for (j = 0; j < sitesMol; j ++) {
    mInert.x += Sqr (mSite[j].r.y) + Sqr (mSite[j].r.z);
    mInert.y += Sqr (mSite[j].r.z) + Sqr (mSite[j].r.x);
    mInert.z += Sqr (mSite[j].r.x) + Sqr (mSite[j].r.y);
  }
  VScale (mInert, 1. / sitesMol);
}


void ApplyThermostat ()
{
  RMat mc, mt;
  VecR vt, waB, wvB;
  real s1, s2, vFac;
  int n;

  s1 = s2 = 0.;
  DO_MOL {
    VSAdd (vt, mol[n].rv, 0.5 * deltaT, mol[n].ra);
    s1 += VDot (vt, mol[n].ra);
    s2 += VLenSq (vt);
    VSAdd (vt, mol[n].wv, 0.5 * deltaT, mol[n].wa);
    MVMulT (wvB, mol[n].rMatT.u, vt);
    MVMulT (waB, mol[n].rMatT.u, mol[n].wa);
    s1 += VWDot (mInert, wvB, waB);
    s2 += VWLenSq (mInert, wvB);
  }
  vFac = - s1 / s2;
  DO_MOL {
    VSAdd (vt, mol[n].rv, 0.5 * deltaT, mol[n].ra);
    VVSAdd (mol[n].ra, vFac, vt);
    VSAdd (vt, mol[n].wv, 0.5 * deltaT, mol[n].wa);
    VVSAdd (mol[n].wa, vFac, vt);
  }
}


void LeapfrogStep (int part)
{
  RMat mc, mt;
  VecR t;
  int n;

  if (part == 1) {
    DO_MOL {
      VVSAdd (mol[n].wv, 0.5 * deltaT, mol[n].wa);
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
    }
    DO_MOL {
      VSCopy (t, 0.5 * deltaT, mol[n].wv);
      BuildStepRmatT (&mc, &t);
      MulMat (mt.u, mc.u, mol[n].rMatT.u, 3);
      mol[n].rMatT = mt;
    }
    DO_MOL VVSAdd (mol[n].r, deltaT, mol[n].rv);
  } else {
    DO_MOL {
      VVSAdd (mol[n].wv, 0.5 * deltaT, mol[n].wa);
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
    }
  }
}


void AdjustTemp ()
{
  VecR wvB;
  real vFac;
  int n;

  vvSum = 0.;
  DO_MOL vvSum += VLenSq (mol[n].rv);
  vFac = velMag / sqrt (vvSum / nMol);
  DO_MOL VScale (mol[n].rv, vFac);
  vvSum = 0.;
  DO_MOL {
    MVMulT (wvB, mol[n].rMatT.u, mol[n].wv);
    vvSum += VWLenSq (mInert, wvB);
  }
  vFac = velMag / sqrt (vvSum / nMol);
  DO_MOL VScale (mol[n].wv, vFac);
}


void InitCoordsWalls (real border)
{
  VecR c, gap, regionI;
  int n, nx, ny, nz;

  VAddCon (regionI, region, - 2. * border);
  VDiv (gap, regionI, initUcell);
  n = 0;
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.5, ny + 0.5, nz + 0.5);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, regionI);
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


void InitAngCoords ()
{
  Quat qe;
  VecR e;
  real eAng[3];
  int n;

  DO_MOL {
    VRand (&e);
    eAng[0] = atan2 (e.x, e.y);
    eAng[1] = acos (e.z);
    eAng[2] = 2. * M_PI * RandR ();
    EulerToQuat (&qe, eAng);
    BuildRotMatrix (&mol[n].rMatT, &qe, 1);
  }
}

void InitAngVels ()
{
  VecR e, wvB;
  real f;
  int n;

  DO_MOL {
    VRand (&e);
    f = velMag / sqrt (VWLenSq (mInert, e));
    VSCopy (wvB, f, e);
    MVMul (mol[n].wv, mol[n].rMatT.u, wvB);
  }
}

void InitAngAccels ()
{
  int n;

  DO_MOL VZero (mol[n].wa);
}


void BuildRotMatrix (RMat *rMat, Quat *q, int transpose)
{
  real p[10], tq[4], s;
  int k, k1, k2;

  tq[0] = q->u1;
  tq[1] = q->u2;
  tq[2] = q->u3;
  tq[3] = q->u4;
  for (k = 0, k2 = 0; k2 < 4; k2 ++) {
    for (k1 = k2; k1 < 4; k1 ++, k ++)
       p[k] = 2. * tq[k1] * tq[k2];
  }
  rMat->u[0] = p[0] + p[9] - 1.;
  rMat->u[4] = p[4] + p[9] - 1.;
  rMat->u[8] = p[7] + p[9] - 1.;
  s = transpose ? 1. : -1.;
  rMat->u[1] = p[1] + s * p[8];
  rMat->u[3] = p[1] - s * p[8];
  rMat->u[2] = p[2] - s * p[6];
  rMat->u[6] = p[2] + s * p[6];
  rMat->u[5] = p[5] + s * p[3];
  rMat->u[7] = p[5] - s * p[3];
}

void EulerToQuat (Quat *qe, real *eAng)
{
  real a1, a2, a3;
  a1 = 0.5 * eAng[1];
  a2 = 0.5 * (eAng[0] - eAng[2]);
  a3 = 0.5 * (eAng[0] + eAng[2]);
  QSet (*qe, sin (a1) * cos (a2), sin (a1) * sin (a2),
     cos (a1) * sin (a3), cos (a1) * cos (a3));
}


void EvalProps ()
{
  VecR wvB;
  real vv, vvMax, vvrMax, vvwMax;
  int n;

  VZero (vSum);
  vvSum = 0.;
  vvrMax = 0.;
  vvwMax = 0.;
  DO_MOL {
    VVAdd (vSum, mol[n].rv);
    vv = VLenSq (mol[n].rv);
    vvSum += vv;
    vvrMax = Max (vvrMax, vv);
    MVMulT (wvB, mol[n].rMatT.u, mol[n].wv);
    vvSum += VWLenSq (mInert, wvB);
    vv = VLenSq (wvB);
    vvwMax = Max (vvwMax, vv);
  }
  vvMax = Sqr (sqrt (vvrMax) + farSiteDist * sqrt (vvwMax));
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


#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

