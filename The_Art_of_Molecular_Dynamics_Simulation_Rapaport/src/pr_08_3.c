
/* [[pr_08_3 - water, H-bonds]] */


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
  VecR r, rv, ra, ra1, ra2, ro, rvo;
  Quat q, qv, qa, qa1, qa2, qo, qvo;
  VecR torq;
  int nBond;
} Mol;
typedef struct {
  VecR f, r;
} Site;
typedef struct {
  VecR r;
  int typeF;
} MSite;

Mol *mol;
Site *site;
MSite *mSite;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
VecR mInert;
real bCon, vvqSum;
int sitesMol;
int stepAdjustTemp;
real *histBondNum, *histPairEng, boundPairEng, maxPairEng, minPairEng;
int countPairEng, limitPairEng, sizeHistBondNum, sizeHistPairEng;

NameList nameList[] = {
  NameR (boundPairEng),
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (limitPairEng),
  NameR (maxPairEng),
  NameR (minPairEng),
  NameR (rCut),
  NameI (sizeHistBondNum),
  NameI (sizeHistPairEng),
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
  PredictorStep ();
  PredictorStepQ ();
  GenSiteCoords ();
  ComputeSiteForces ();
  ComputeTorqs ();
  ComputeAccelsQ ();
  ApplyThermostat ();
  CorrectorStep ();
  CorrectorStepQ ();
  AdjustQuat ();
  ApplyBoundaryCond ();
  EvalProps ();
  if (stepCount % stepAdjustTemp == 0) AdjustTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (countPairEng == limitPairEng) PrintPairEng (stdout);
}

void SetupJob ()
{
  AllocArrays ();
  DefineMol ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  InitAngCoords ();
  InitAngVels ();
  InitAngAccels ();
  AccumProps (0);
  InitPairEng ();
}

void SetParams ()
{
  sitesMol = 4;
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nMol = VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (site, nMol * sitesMol, Site);
  AllocMem (mSite, sitesMol, MSite);
  AllocMem (histBondNum, sizeHistBondNum, real);
  AllocMem (histPairEng, sizeHistPairEng, real);
}


void ComputeSiteForces ()
{
  VecR dr, shift;
  real fcVal, rr, rrCut, rri, rri3, uVal;
  int j1, j2, m1, m2, ms1, ms2, n, typeSum;
  real uSumPair;
  int j;

  rrCut = Sqr (rCut);
  for (n = 0; n < nMol * sitesMol; n ++) VZero (site[n].f);
  DO_MOL mol[n].nBond = 0;
  uSum = 0.;
  for (m1 = 0; m1 < nMol - 1; m1 ++) {
    for (m2 = m1 + 1; m2 < nMol; m2 ++) {
      VSub (dr, mol[m1].r, mol[m2].r);
      VZero (shift);
      VShiftAll (dr);
      VVAdd (dr, shift);
      rr = VLenSq (dr);
      if (rr < rrCut) {
        uSumPair = 0.;
        ms1 = m1 * sitesMol;
        ms2 = m2 * sitesMol;
        for (j1 = 0; j1 < sitesMol; j1 ++) {
          for (j2 = 0; j2 < sitesMol; j2 ++) {
            typeSum = mSite[j1].typeF + mSite[j2].typeF;
            if (mSite[j1].typeF == mSite[j2].typeF || typeSum == 5) {
              VSub (dr, site[ms1 + j1].r, site[ms2 + j2].r);
              VVAdd (dr, shift);
              rr = VLenSq (dr);
              rri = 1. / rr;
              switch (typeSum) {
                case 2:
                  rri3 = Cube (rri);
                  uVal = 4. * rri3 * (rri3 - 1.);
                  fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
                  break;
                case 4:
                  uVal = 4. * bCon * sqrt (rri);
                  fcVal = uVal * rri;
                  break;
                case 5:
                  uVal = -2. * bCon * sqrt (rri);
                  fcVal = uVal * rri;
                  break;
                case 6:
                  uVal = bCon * sqrt (rri);
                  fcVal = uVal * rri;
                  break;
              }
              VVSAdd (site[ms1 + j1].f, fcVal, dr);
              VVSAdd (site[ms2 + j2].f, - fcVal, dr);
              uSumPair += uVal;
            }
          }
        }
        uSum += uSumPair;
        j = sizeHistPairEng * (uSumPair - minPairEng) /
           (maxPairEng - minPairEng);
        ++ histPairEng[Clamp (j, 0, sizeHistPairEng - 1)];
        if (uSumPair < boundPairEng) {
          ++ mol[m1].nBond;
          ++ mol[m2].nBond;
        }
      }
    }
  }
  DO_MOL ++ histBondNum[Min (mol[n].nBond, sizeHistBondNum - 1)];
  ++ countPairEng;
}


void GenSiteCoords ()
{
  RMat rMat;
  VecR t;
  int j, n;

  DO_MOL {
    BuildRotMatrix (&rMat, &mol[n].q, 1);
    for (j = 0; j < sitesMol; j ++) {
      MVMul (t, rMat.u, mSite[j].r);
      VAdd (site[sitesMol * n + j].r, mol[n].r, t);
    }
  }
}

void ComputeTorqs ()
{
  RMat rMat;
  VecR dr, t, torqS;
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
    BuildRotMatrix (&rMat, &mol[n].q, 0);
    MVMul (mol[n].torq, rMat.u, torqS);
  }
}

void ComputeAccelsQ ()
{
  Quat qs;
  VecR w;
  int n;

  DO_MOL {
    ComputeAngVel (n, &w);
    QSet (qs,
       (mol[n].torq.x + (mInert.y - mInert.z) * w.y * w.z) / mInert.x,
       (mol[n].torq.y + (mInert.z - mInert.x) * w.z * w.x) / mInert.y,
       (mol[n].torq.z + (mInert.x - mInert.y) * w.x * w.y) / mInert.z,
       - 2. * QLenSq (mol[n].qv));
    QMul (mol[n].qa, mol[n].q, qs);
    QScale (mol[n].qa, 0.5);
  }
}

void ComputeAngVel (int n, VecR *w)
{
  Quat qt, qvt;

  qvt = mol[n].qv;
  qvt.u4 *= -1.;
  QMul (qt, qvt, mol[n].q);
  QScale (qt, 2.);
  VSet (*w, qt.u1, qt.u2, qt.u3);
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


void DefineMol ()
{
  int j;

  for (j = 0; j < sitesMol; j ++) VZero (mSite[j].r);
  mSite[0].r.z = -0.0206;
  mSite[1].r.z = 0.0274;
  mSite[2].r.y = 0.240;
  mSite[2].r.z = 0.165;
  mSite[3].r.y = - mSite[2].r.y;
  mSite[3].r.z = mSite[2].r.z;
  VSet (mInert, 0.00980, 0.00340, 0.00640);
  bCon = 183.5;
  mSite[0].typeF = 1;
  mSite[1].typeF = 2;
  mSite[2].typeF = 3;
  mSite[3].typeF = 3;
}


#define PCR4(r, ro, v, a, a1, a2, t)                        \
   r.t = ro.t + deltaT * v.t +                              \
   wr * (cr[0] * a.t + cr[1] * a1.t + cr[2] * a2.t)
#define PCV4(r, ro, v, a, a1, a2, t)                        \
   v.t = (r.t - ro.t) / deltaT +                            \
   wv * (cv[0] * a.t + cv[1] * a1.t + cv[2] * a2.t)

#define PR(t)                                               \
   PCR4 (mol[n].r, mol[n].r, mol[n].rv,                     \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)
#define PRV(t)                                              \
   PCV4 (mol[n].r, mol[n].ro, mol[n].rv,                    \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)
#define CR(t)                                               \
   PCR4 (mol[n].r, mol[n].ro, mol[n].rvo,                   \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)
#define CRV(t)                                              \
   PCV4 (mol[n].r, mol[n].ro, mol[n].rv,                    \
   mol[n].ra, mol[n].ra1, mol[n].ra2, t)

void PredictorStep ()
{
  real cr[] = {19.,-10.,3.}, cv[] = {27.,-22.,7.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    mol[n].ro = mol[n].r;
    mol[n].rvo = mol[n].rv;
    PR (x);
    PRV (x);
    PR (y);
    PRV (y);
    PR (z);
    PRV (z);
    mol[n].ra2 = mol[n].ra1;
    mol[n].ra1 = mol[n].ra;
  }
}

void CorrectorStep ()
{
  real cr[] = {3.,10.,-1.}, cv[] = {7.,6.,-1.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    CR (x);
    CRV (x);
    CR (y);
    CRV (y);
    CR (z);
    CRV (z);
  }
}


#define PQ(t)                                               \
   PCR4 (mol[n].q, mol[n].q, mol[n].qv,                     \
   mol[n].qa, mol[n].qa1, mol[n].qa2, t)
#define PQV(t)                                              \
   PCV4 (mol[n].q, mol[n].qo, mol[n].qv,                    \
   mol[n].qa, mol[n].qa1, mol[n].qa2, t)
#define CQ(t)                                               \
   PCR4 (mol[n].q, mol[n].qo, mol[n].qvo,                   \
   mol[n].qa, mol[n].qa1, mol[n].qa2, t)
#define CQV(t)                                              \
   PCV4 (mol[n].q, mol[n].qo, mol[n].qv,                    \
   mol[n].qa, mol[n].qa1, mol[n].qa2, t)

void PredictorStepQ ()
{
  real cr[] = {19.,-10.,3.}, cv[] = {27.,-22.,7.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    mol[n].qo = mol[n].q;
    mol[n].qvo = mol[n].qv;
    PQ (u1);
    PQV (u1);
    PQ (u2);
    PQV (u2);
    PQ (u3);
    PQV (u3);
    PQ (u4);
    PQV (u4);
    mol[n].qa2 = mol[n].qa1;
    mol[n].qa1 = mol[n].qa;
  }
}

void CorrectorStepQ ()
{
  real cr[] = {3.,10.,-1.}, cv[] = {7.,6.,-1.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    CQ (u1);
    CQV (u1);
    CQ (u2);
    CQV (u2);
    CQ (u3);
    CQV (u3);
    CQ (u4);
    CQV (u4);
  }
}


void ApplyBoundaryCond ()
{
  int n;

  DO_MOL VWrapAll (mol[n].r);
}


void AdjustTemp ()
{
  real vFac;
  VecR w;
  int n;

  vvSum = 0.;
  DO_MOL vvSum += VLenSq (mol[n].rv);
  vFac = velMag / sqrt (vvSum / nMol);
  DO_MOL VScale (mol[n].rv, vFac);
  vvqSum = 0.;
  DO_MOL {
    ComputeAngVel (n, &w);
    vvqSum += VWLenSq (mInert, w);
  }
  vFac = velMag / sqrt (vvqSum / nMol);
  DO_MOL QScale (mol[n].qv, vFac);
}

void ApplyThermostat ()
{
  real s1, s2, vFac;
  VecR w;
  int n;

  s1 = s2 = 0.;
  DO_MOL {
    s1 += VDot (mol[n].rv, mol[n].ra);
    s2 += VLenSq (mol[n].rv);
  }
  DO_MOL {
    ComputeAngVel (n, &w);
    s1 += VDot (w, mol[n].torq);
    s2 += VWLenSq (mInert, w);
  }
  vFac = - s1 / s2;
  DO_MOL {
    VVSAdd (mol[n].ra, vFac, mol[n].rv);
    QSAdd (mol[n].qa, mol[n].qa, vFac, mol[n].qv);
  }
}

void AdjustQuat ()
{
  real qi;
  int n;

  DO_MOL {
    qi = 1. / sqrt (QLenSq (mol[n].q));
    QScale (mol[n].q, qi);
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

  DO_MOL {
    VZero (mol[n].ra);
    VZero (mol[n].ra1);
    VZero (mol[n].ra2);
  }
}


void InitAngCoords ()
{
  VecR e;
  real eAng[3];
  int n;

  DO_MOL {
    VRand (&e);
    eAng[0] = atan2 (e.x, e.y);
    eAng[1] = acos (e.z);
    eAng[2] = 2. * M_PI * RandR ();
    EulerToQuat (&mol[n].q, eAng);
  }
}

void InitAngVels ()
{
  Quat qe;
  VecR e;
  real f;
  int n;

  DO_MOL {
    VRand (&e);
    QSet (qe, e.x, e.y, e.z, 0.);
    QMul (mol[n].qv, mol[n].q, qe);
    f = 0.5 * velMag / sqrt (VWLenSq (mInert, e));
    QScale (mol[n].qv, f);
  }
}

void InitAngAccels ()
{
  int n;

  DO_MOL {
    QZero (mol[n].qa);
    QZero (mol[n].qa1);
    QZero (mol[n].qa2);
  }
}


void EvalProps ()
{
  VecR w;
  int n;

  VZero (vSum);
  vvSum = 0.;
  DO_MOL {
    VVAdd (vSum, mol[n].rv);
    vvSum += VLenSq (mol[n].rv);
  }
  vvqSum = 0.;
  DO_MOL {
    ComputeAngVel (n, &w);
    vvqSum += VWLenSq (mInert, w);
  }
  vvSum += vvqSum;
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


void InitPairEng ()
{
  int n;

  for (n = 0; n < sizeHistPairEng; n ++) histPairEng[n] = 0.;
  for (n = 0; n < sizeHistBondNum; n ++) histBondNum[n] = 0.;
  countPairEng = 0;
}

void PrintPairEng (FILE *fp)
{
  real eVal, hSum;
  int n;

  hSum = 0.;
  for (n = 0; n < sizeHistPairEng; n ++) hSum += histPairEng[n];
  for (n = 0; n < sizeHistPairEng; n ++) histPairEng[n] /= hSum;
  hSum = 0.;
  for (n = 0; n < sizeHistBondNum; n ++) hSum += histBondNum[n];
  for (n = 0; n < sizeHistBondNum; n ++) histBondNum[n] /= hSum;
  fprintf (fp, "pair energy\n");
  for (n = 0; n < sizeHistPairEng; n ++) {
    eVal = minPairEng + (n + 0.5) * (maxPairEng - minPairEng) /
        sizeHistPairEng;
    fprintf (fp, "%8.4f %8.4f\n", eVal, histPairEng[n]);
  }
  fprintf (fp, "bond count\n");
  for (n = 0; n < sizeHistBondNum; n ++)
     fprintf (fp, "%d %8.4f\n", n, histBondNum[n]);
  fflush (fp);
  InitPairEng ();
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

