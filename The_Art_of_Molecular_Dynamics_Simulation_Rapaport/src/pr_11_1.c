
/* [[pr_11_1 - chain equilibrium]] */


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
  RMat rMatT;
  VecR r, rv, omega, omegah, bV, cV, hV;
  real inertiaM[9], fV[6], gV[6], xV[6], yV[6], mass, s, sv, svh, sa, torq;
} Link;
typedef struct {
  Link *L;
  VecR ra, wa;
  int nLink;
} Poly;
typedef struct {
  VecR f, r;
} Site;

Poly P;
Site *site;
VecR region;
VecI cells;
real deltaT, rCut, temperature, timeNow, uSum;
Prop kinEnergy, totEnergy;
int moreCycles, stepAvg, stepCount, stepEquil, stepLimit;
real bondAng, bondLen, kinEnVal, totEnVal, twistAng, uCon;
int *cellList, chainLen, helixPeriod, nDof, nSite;

NameList nameList[] = {
  NameR (bondLen),
  NameI (chainLen),
  NameR (deltaT),
  NameI (helixPeriod),
  NameR (region),
  NameI (stepAvg),
  NameI (stepEquil),
  NameI (stepLimit),
  NameR (temperature),
  NameR (uCon),
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
  LeapfrogStepLinks (1);
  AdjustLinkAngles ();
  ComputeLinkCoordsVels ();
  BuildLinkInertiaMats ();
  ComputeSiteForces ();
  ComputeWallForces ();
  ComputeLinkForces ();
  ComputeLinkAccels ();
  LeapfrogStepLinks (2);
  EvalProps ();
  if (stepCount < stepEquil && stepCount % 100 == 0) AdjustTemp ();
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
  InitLinkState ();
  AccumProps (0);
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  nSite = chainLen + 1;
  nDof = chainLen + 4;
  bondAng = 2. * M_PI / helixPeriod;
  twistAng = asin (1.1 * rCut / (helixPeriod * bondLen));
  VSCopy (cells, 1. / rCut, region);
}

void AllocArrays ()
{
  AllocMem (P.L, chainLen, Link);
  AllocMem (site, nSite, Site);
  AllocMem (cellList, VProd (cells) + nSite, int);
}


void ComputeSiteForces ()
{
  VecR dr, invWid, rs;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real fcVal, rr, rrCut, rri, rri3;
  int c, j1, j2, m1, m1x, m1y, m1z, m2, n, offset;

  for (n = 0; n < nSite; n ++) VZero (site[n].f);
  uSum = 0.;
  VDiv (invWid, cells, region);
  for (n = nSite; n < nSite + VProd (cells); n ++) cellList[n] = -1;
  for (n = 0; n < nSite; n ++) {
    VSAdd (rs, site[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nSite;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
  rrCut = Sqr (rCut);
  for (m1z = 0; m1z < cells.z; m1z ++) {
    for (m1y = 0; m1y < cells.y; m1y ++) {
      for (m1x = 0; m1x < cells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        m1 = VLinear (m1v, cells) + nSite;
        if (cellList[m1] < 0) continue;
        for (offset = 0; offset < N_OFFSET; offset ++) {
          VAdd (m2v, m1v, vOff[offset]);
          if (m2v.x < 0 || m2v.x >= cells.x ||
              m2v.y < 0 || m2v.y >= cells.y ||
                           m2v.z >= cells.z) continue;
          m2 = VLinear (m2v, cells) + nSite;
          if (cellList[m2] < 0) continue;
          DO_CELL (j1, m1) {
            DO_CELL (j2, m2) {
              if ((m1 != m2 || j2 < j1) && abs (j1 - j2) > 3) {
                VSub (dr, site[j1].r, site[j2].r);
                rr = VLenSq (dr);
                if (rr < rrCut) {
                  rri = 1. / rr;
                  rri3 = Cube (rri);
                  fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
                  VVSAdd (site[j1].f, fcVal, dr);
                  VVSAdd (site[j2].f, - fcVal, dr);
                  uSum += 4. * rri3 * (rri3 - 1.) + 1.;
                }
              }
            }
          }
        }
      }
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

void ComputeWallForces ()
{
  real dr, rri, rri3;
  int j;

  for (j = 0; j < nSite; j ++) {
    WallForce (x);
    WallForce (y);
    WallForce (z);
  }
}


void ComputeLinkCoordsVels ()
{
  RMat rMat;
  VecR bEx, bVp, hVp;
  real phiT[36], vs[6], vsp[6];
  int k;

  VSet (bVp, 0., 0., bondLen);
  VSet (hVp, 0., 0., 1.);
  for (k = 0; k < P.nLink; k ++) {
    if (k > 0) {
      MVMul (P.L[k].hV, P.L[k - 1].rMatT.u, hVp);
      BuildLinkRotmatT (&rMat, P.L[k].s, bondAng);
      MulMat (P.L[k].rMatT.u, P.L[k - 1].rMatT.u, rMat.u, 3);
    }
    MVMul (P.L[k].bV, P.L[k].rMatT.u, bVp);
  }
  for (k = 0; k < P.nLink; k ++) {
    VToLin (vs, 0, P.L[k].omega);
    VToLin (vs, 3, P.L[k].rv);
    BuildLinkPhimatT (phiT, k);
    MulMatVec (vsp, phiT, vs, 6);
    if (k < P.nLink - 1) {
      VFromLin (P.L[k + 1].omega, vsp, 0);
      VVSAdd (P.L[k + 1].omega, P.L[k + 1].sv, P.L[k + 1].hV);
    }
    VFromLin (P.L[k + 1].rv, vsp, 3);
  }
  for (k = 0; k < P.nLink; k ++)
     VAdd (P.L[k + 1].r, P.L[k].r, P.L[k].bV);
  for (k = 0; k < P.nLink + 1; k ++) site[k + 1].r = P.L[k].r;
  VSet (bEx, 0., - sin (bondAng), - cos (bondAng));
  VScale (bEx, bondLen);
  MVMul (site[0].r, P.L[0].rMatT.u, bEx);
  VVAdd (site[0].r, site[1].r);
}

void ComputeLinkForces ()
{
  VecR d, fc, tq, tq1;
  real ang;
  int k;

  for (k = 1; k < P.nLink; k ++) {
    ang = P.L[k].s - twistAng;
    P.L[k].torq = - uCon * sin (ang);
    uSum -= uCon * cos (ang);
  }
  for (k = 0; k < P.nLink; k ++) {
    fc = site[k + 2].f;
    VCross (tq, P.L[k].bV, fc);
    if (k == 0) {
      VVAdd (fc, site[1].f);
      VVAdd (fc, site[0].f);
      VSub (d, site[0].r, site[1].r);
      VCross (tq1, d, site[0].f);
      VVAdd (tq, tq1);
    }
    VToLin (P.L[k].fV, 0, tq);
    VToLin (P.L[k].fV, 3, fc);
  }
}

#define Mn(a, i, j)  MAT (a, n, i, j)
#define M3(a, i, j)  MAT (a, 3, i, j)
#define M6(a, i, j)  MAT (a, 6, i, j)

#define DO(m, n)  for (m = 0; m < n; m ++)

void ComputeLinkAccels ()
{
  real as[6], asp[6], h[3], mMat[36], phi[36], phiT[36], pMat[36],
     tMat1[36], tMat2[36], z[6], zp[6], zt[6], dk, e;
  int i, j, k;

  DO (i, 6) zp[i] = 0.;
  for (k = P.nLink - 1; k >= 0; k --) {
    BuildLinkPhimatT (phiT, k);
    DO (i, 6) {
      DO (j, 6) M6 (phi, i, j) = M6 (phiT, j, i);
    }
    BuildLinkXYvecs (k);
    BuildLinkMmat ((k == P.nLink - 1) ? pMat : mMat, k);
    if (k < P.nLink - 1) {
      DO (i, 6) {
        DO (j, 6) M6 (tMat1, i, j) = (i == j) ? 1. : 0.;
      }
      VToLin (h, 0, P.L[k + 1].hV);
      DO (i, 6) {
        DO (j, 3) M6 (tMat1, i, j) -= P.L[k + 1].gV[i] * h[j];
      }
      MulMat (tMat2, tMat1, pMat, 6);
      MulMat (tMat1, tMat2, phiT, 6);
      MulMat (pMat, phi, tMat1, 6);
      DO (i, 6) {
        DO (j, 6) M6 (pMat, i, j) += M6 (mMat, i, j);
      }
    }
    if (k > 0) {
      VToLin (h, 0, P.L[k].hV);
      dk = 0.;
      DO (i, 3) {
        DO (j, 3) dk += h[i] * M6 (pMat, i, j) * h[j];
      }
    }
    MulMatVec (z, phi, zp, 6);
    DO (i, 6) z[i] += P.L[k].yV[i] - P.L[k].fV[i];
    if (k > 0) {
      DO (i, 6) {
        P.L[k].gV[i] = 0.;
        DO (j, 3) P.L[k].gV[i] += M6 (pMat, i, j) * h[j];
        P.L[k].gV[i] /= dk;
      }
      MulMatVec (zt, pMat, P.L[k].xV, 6);
      DO (i, 6) z[i] += zt[i];
      e = P.L[k].torq;
      DO (i, 3) e -= h[i] * z[i];
      P.L[k].sa = e / dk;
      DO (i, 6) zp[i] = z[i] + e * P.L[k].gV[i];
    }
  }
  SolveLineq (pMat, z, 6);
  DO (i, 6) as[i] = - z[i];
  VFromLin (P.wa, as, 0);
  VFromLin (P.ra, as, 3);
  for (k = 1; k < P.nLink; k ++) {
    BuildLinkPhimatT (phiT, k - 1);
    MulMatVec (asp, phiT, as, 6);
    DO (i, 6) P.L[k].sa -= P.L[k].gV[i] * asp[i];
    if (k < P.nLink - 1) {
      DO (i, 6) as[i] = asp[i] + P.L[k].xV[i];
      VToLin (h, 0, P.L[k].hV);
      DO (i, 3) as[i] += h[i] * P.L[k].sa;
    }
  }
}

void BuildLinkXYvecs (int k)
{
  VecR dv, w, w1, w2;
  int i;

  if (k > 0) {
    VCross (w, P.L[k - 1].omega, P.L[k].hV);
    VScale (w, P.L[k].sv);
    VToLin (P.L[k].xV, 0, w);
    VSub (dv, P.L[k].rv, P.L[k - 1].rv);
    VCross (w, P.L[k - 1].omega, dv);
    VToLin (P.L[k].xV, 3, w);
  } else {
    DO (i, 6) P.L[k].xV[i] = 0.;
  }
  MVMul (w, P.L[k].inertiaM, P.L[k].omega);
  VCross (w1, P.L[k].omega, w);
  VToLin (P.L[k].yV, 0, w1);
  VCross (w, P.L[k].omega, P.L[k].cV);
  VCross (w2, P.L[k].omega, w);
  VScale (w2, P.L[k].mass);
  VToLin (P.L[k].yV, 3, w2);
}

void BuildLinkMmat (real *mMat, int k)
{
  VecR w;
  int i, j;

  DO (i, 6) {
    DO (j, 6) {
      if (i < 3 && j < 3)
         M6 (mMat, i, j) = M3 (P.L[k].inertiaM, i, j);
      else M6 (mMat, i, j) = (i == j) ? P.L[k].mass : 0.;
    }
  }
  VSCopy (w, P.L[k].mass, P.L[k].cV);
  M6 (mMat, 2, 4) = w.x;
  M6 (mMat, 1, 5) = - w.x;
  M6 (mMat, 0, 5) = w.y;
  M6 (mMat, 2, 3) = - w.y;
  M6 (mMat, 1, 3) = w.z;
  M6 (mMat, 0, 4) = - w.z;
  M6 (mMat, 4, 2) = M6 (mMat, 2, 4);
  M6 (mMat, 5, 1) = M6 (mMat, 1, 5);
  M6 (mMat, 5, 0) = M6 (mMat, 0, 5);
  M6 (mMat, 3, 2) = M6 (mMat, 2, 3);
  M6 (mMat, 3, 1) = M6 (mMat, 1, 3);
  M6 (mMat, 4, 0) = M6 (mMat, 0, 4);
}

void BuildLinkInertiaMats ()
{
  VecR d;
  real dd, iBall, inertiaK;
  int k;

  inertiaK = 0.1;
  for (k = 0; k < P.nLink; k ++) {
    if (k > 0) {
      P.L[k].mass = 1.;
      VSub (P.L[k].cV, site[k + 2].r, site[k + 1].r);
    } else {
      P.L[k].mass = 3.;
      VAdd (P.L[k].cV, site[2].r, site[1].r);
      VVAdd (P.L[k].cV, site[0].r);
      VScale (P.L[k].cV, 1./3.);
      VVSub (P.L[k].cV, site[1].r);
    }
    iBall = inertiaK * P.L[k].mass;
    VSub (d, site[k + 2].r, site[k + 1].r);
    dd = VLenSq (d);
    M3 (P.L[k].inertiaM, 0, 0) = dd - Sqr (d.x) + iBall;
    M3 (P.L[k].inertiaM, 1, 1) = dd - Sqr (d.y) + iBall;
    M3 (P.L[k].inertiaM, 2, 2) = dd - Sqr (d.z) + iBall;
    M3 (P.L[k].inertiaM, 0, 1) = - d.x * d.y;
    M3 (P.L[k].inertiaM, 0, 2) = - d.x * d.z;
    M3 (P.L[k].inertiaM, 1, 2) = - d.y * d.z;
    if (k == 0) {
      VSub (d, site[0].r, site[1].r);
      M3 (P.L[k].inertiaM, 0, 0) += dd - Sqr (d.x);
      M3 (P.L[k].inertiaM, 1, 1) += dd - Sqr (d.y);
      M3 (P.L[k].inertiaM, 2, 2) += dd - Sqr (d.z);
      M3 (P.L[k].inertiaM, 0, 1) -= d.x * d.y;
      M3 (P.L[k].inertiaM, 0, 2) -= d.x * d.z;
      M3 (P.L[k].inertiaM, 1, 2) -= d.y * d.z;
    }
    M3 (P.L[k].inertiaM, 1, 0) = M3 (P.L[k].inertiaM, 0, 1);
    M3 (P.L[k].inertiaM, 2, 0) = M3 (P.L[k].inertiaM, 0, 2);
    M3 (P.L[k].inertiaM, 2, 1) = M3 (P.L[k].inertiaM, 1, 2);
  }
}

void LeapfrogStepLinks (int part)
{
  RMat mc, mt;
  VecR t;
  int k;

  if (part == 1) {
    VVSAdd (P.L[0].omega, 0.5 * deltaT, P.wa);
    VVSAdd (P.L[0].rv, 0.5 * deltaT, P.ra);
    for (k = 1; k < P.nLink; k ++)
       P.L[k].sv += 0.5 * deltaT * P.L[k].sa;
    VSCopy (t, 0.5 * deltaT, P.L[0].omega);
    BuildStepRmatT (&mc, &t);
    MulMat (mt.u, mc.u, P.L[0].rMatT.u, 3);
    P.L[0].rMatT = mt;
    VVSAdd (P.L[0].r, deltaT, P.L[0].rv);
    for (k = 1; k < P.nLink; k ++)
       P.L[k].s += deltaT * P.L[k].sv;
    P.L[0].omegah = P.L[0].omega;
    VVSAdd (P.L[0].omega, 0.5 * deltaT, P.wa);
    for (k = 1; k < P.nLink; k ++) {
      P.L[k].svh = P.L[k].sv;
      P.L[k].sv += 0.5 * deltaT * P.L[k].sa;
    }
  } else {
    P.L[0].omega = P.L[0].omegah;
    for (k = 1; k < P.nLink; k ++) P.L[k].sv = P.L[k].svh;
    VVSAdd (P.L[0].omega, 0.5 * deltaT, P.wa);
    VVSAdd (P.L[0].rv, 0.5 * deltaT, P.ra);
    for (k = 1; k < P.nLink; k ++)
       P.L[k].sv += 0.5 * deltaT * P.L[k].sa;
  }
}

void EvalProps ()
{
  VecR w1, w2;
  int k;

  kinEnVal = 0.;
  for (k = 0; k < P.nLink; k ++) {
    MVMul (w2, P.L[k].inertiaM, P.L[k].omega);
    VCross (w1, P.L[k].cV, P.L[k].rv);
    kinEnVal += 0.5 * (P.L[k].mass * (VLenSq (P.L[k].rv) +
       2. * VDot (P.L[k].omega, w1)) + VDot (P.L[k].omega, w2));
  }
  totEnVal = (kinEnVal + uSum) / nDof;
  kinEnVal /= nDof;
}

void AdjustLinkAngles ()
{
  int k;

  for (k = 1; k < P.nLink; k ++) {
    if (P.L[k].s >= 2. * M_PI) P.L[k].s -= 2. * M_PI;
    else if (P.L[k].s < 0.) P.L[k].s += 2. * M_PI;
  }
}

void AdjustTemp ()
{
  real vFac;
  int k;

  vFac = sqrt (temperature / (2. * kinEnVal));
  VScale (P.L[0].rv, vFac);
  VScale (P.L[0].omega, vFac);
  for (k = 1; k < P.nLink; k ++) P.L[k].sv *= vFac;
  ComputeLinkCoordsVels ();
  EvalProps ();
}

void InitLinkState ()
{
  VecR rs, vs, w;
  real mSum;
  int j, k;

  P.nLink = chainLen - 1;
  VZero (P.L[0].r);
  VZero (P.L[0].rv);
  VZero (P.ra);
  DO (j, 9) P.L[0].rMatT.u[j] = (j % 4 == 0) ? 1. : 0.;
  VZero (P.L[0].omega);
  VZero (P.wa);
  for (k = 1; k < P.nLink; k ++) {
    P.L[k].s = M_PI + ((k % 2 == 0) ? 0.42 : -0.4);
    P.L[k].sv = 0.2 * (1. - 2. * RandR ());
    P.L[k].sa = 0.;
  }
  ComputeLinkCoordsVels ();
  BuildLinkInertiaMats ();
  VZero (rs);
  VZero (vs);
  mSum = 0.;
  for (k = 0; k < P.nLink; k ++) {
    VAdd (w, P.L[k].r, P.L[k].cV);
    VVSAdd (rs, P.L[k].mass, w);
    VCross (w, P.L[k].omega, P.L[k].cV);
    VVAdd (w, P.L[k].rv);
    VVSAdd (vs, P.L[k].mass, w);
    mSum += P.L[k].mass;
  }
  VVSAdd (P.L[0].r, -1. / mSum, rs);
  VVSAdd (P.L[0].rv, -1. / mSum, vs);
  ComputeLinkCoordsVels ();
  BuildLinkInertiaMats ();
  ComputeSiteForces ();
  ComputeWallForces ();
  ComputeLinkForces ();
  EvalProps ();
  AdjustTemp ();
}

void BuildLinkRotmatT (RMat *rMat, real dihedA, real bondA)
{
  real cb, cd, sb, sd;

  cb = cos (bondA);
  sb = sin (bondA);
  cd = cos (dihedA);
  sd = sin (dihedA);
  M3 (rMat->u, 0, 0) =   cd;
  M3 (rMat->u, 1, 0) =   sd;
  M3 (rMat->u, 2, 0) =   0.;
  M3 (rMat->u, 0, 1) = - sd * cb;
  M3 (rMat->u, 1, 1) =   cd * cb;
  M3 (rMat->u, 2, 1) =   sb;
  M3 (rMat->u, 0, 2) =   sd * sb;
  M3 (rMat->u, 1, 2) = - cd * sb;
  M3 (rMat->u, 2, 2) =   cb;
}

void BuildLinkPhimatT (real *phiT, int k)
{
  int i, j;

  DO (i, 6) {
    DO (j, 6) M6 (phiT, i, j) = (i == j) ? 1. : 0.;
  }
  M6 (phiT, 3, 1) =   P.L[k].bV.z;
  M6 (phiT, 3, 2) = - P.L[k].bV.y;
  M6 (phiT, 4, 0) = - P.L[k].bV.z;
  M6 (phiT, 4, 2) =   P.L[k].bV.x;
  M6 (phiT, 5, 0) =   P.L[k].bV.y;
  M6 (phiT, 5, 1) = - P.L[k].bV.x;
}

void MulMatVec (real *a, real *b, real *c, int n)
{
  int i, k;

  DO (i, n) {
    a[i] = 0.;
    DO (k, n) a[i] += Mn (b, i, k) * c[k];
  }
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


void AccumProps (int icode)
{
  if (icode == 0) {
    PropZero (totEnergy);
    PropZero (kinEnergy);
  } else if (icode == 1) {
    totEnergy.val = totEnVal;
    kinEnergy.val = kinEnVal;
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
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f",
     stepCount, timeNow, PropEst (totEnergy),
     PropEst (kinEnergy));
  fprintf (fp, "\n");
  fflush (fp);
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

