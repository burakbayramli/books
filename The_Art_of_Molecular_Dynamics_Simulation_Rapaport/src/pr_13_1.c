
/* [[pr_13_1 - dipolar soft-sphere fluid]] */


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
  VecR s, sv, sa, sa1, sa2, so, svo;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, randSeed, stepAvg, stepCount, stepEquil, stepLimit;
VecR **tCos, **tSin;
real alpha, dipoleInt, mInert, vvsSum;
int fSpaceLimit;
Prop dipoleOrder;
int stepAdjustTemp;
real **histRdf, rangeRdf;
int countRdf, limitRdf, sizeHistRdf, stepRdf;

NameList nameList[] = {
  NameR (alpha),
  NameR (deltaT),
  NameR (density),
  NameR (dipoleInt),
  NameI (fSpaceLimit),
  NameI (initUcell),
  NameI (limitRdf),
  NameR (mInert),
  NameI (randSeed),
  NameR (rangeRdf),
  NameI (sizeHistRdf),
  NameI (stepAdjustTemp),
  NameI (stepAvg),
  NameI (stepEquil),
  NameI (stepLimit),
  NameI (stepRdf),
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
  PredictorStepS ();
  ComputeForces ();
  ComputeForcesDipoleR ();
  ComputeForcesDipoleF ();
  ComputeDipoleAccel ();
  ApplyThermostat ();
  CorrectorStep ();
  CorrectorStepS ();
  AdjustDipole ();
  ApplyBoundaryCond ();
  EvalProps ();
  if (stepCount % stepAdjustTemp == 0) AdjustTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (stepCount >= stepEquil && (stepCount - stepEquil) % stepRdf == 0)
     EvalRdf ();
}

void SetupJob ()
{
  AllocArrays ();
  InitRand (randSeed);
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  InitAngCoords ();
  InitAngVels ();
  InitAngAccels ();
  AccumProps (0);
  countRdf = 0;
}

void SetParams ()
{
  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density / 4., 1./3.), initUcell);
  nMol = 4 * VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
}

void AllocArrays ()
{
  int k;

  AllocMem (mol, nMol, Mol);
  AllocMem2 (tCos, fSpaceLimit + 1, nMol, VecR);
  AllocMem2 (tSin, fSpaceLimit + 1, nMol, VecR);
  AllocMem2 (histRdf, 3, sizeHistRdf, real);
}

void PrintSummary (FILE *fp)
{
  fprintf (fp,
     "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy));
  fprintf (fp, " %7.4f %7.4f", PropEst (dipoleOrder));
  fprintf (fp, "\n");
  fflush (fp);
}


void ComputeForces ()
{
  VecR dr;
  real fcVal, rr, rrCut, rri, rri3;
  int j1, j2, n;

  rrCut = Sqr (rCut);
  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  for (j1 = 0; j1 < nMol - 1; j1 ++) {
    for (j2 = j1 + 1; j2 < nMol; j2 ++) {
      VSub (dr, mol[j1].r, mol[j2].r);
      VWrapAll (dr);
      rr = VLenSq (dr);
      if (rr < rrCut) {
        rri = 1. / rr;
        rri3 = Cube (rri);
        fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
        VVSAdd (mol[j1].ra, fcVal, dr);
        VVSAdd (mol[j2].ra, - fcVal, dr);
        uSum += 4. * rri3 * (rri3 - 1.) + 1.;
      }
    }
  }
}


void ComputeForcesDipoleR ()
{
  VecR dr, w;
  real a1, a2, a3, alpha2, d, irPi, rr, rrCut, rri, sr1, sr2, ss, t;
  int j1, j2, n;

  rrCut = Sqr (0.5 * region.x);
  irPi = 1. / sqrt (M_PI);
  alpha2 = Sqr (alpha);
  DO_MOL VZero (mol[n].sa);
  for (j1 = 0; j1 < nMol - 1; j1 ++) {
    for (j2 = j1 + 1; j2 < nMol; j2 ++) {
      VSub (dr, mol[j1].r, mol[j2].r);
      VWrapAll (dr);
      rr = VLenSq (dr);
      if (rr < rrCut) {
        d = sqrt (rr);
        rri = 1. / rr;
        t = 2. * dipoleInt * alpha * exp (- alpha2 * rr) * rri * irPi;
        a1 = dipoleInt * erfc (alpha * d) * rri / d + t;
        a2 = 3. * a1 * rri + 2. * alpha2 * t;
        a3 = 5. * a2 * rri + 4. * Sqr (alpha2) * t;
        ss = VDot (mol[j1].s, mol[j2].s);
        sr1 = VDot (mol[j1].s, dr);
        sr2 = VDot (mol[j2].s, dr);
        VSSAdd (w, sr2, mol[j1].s, sr1, mol[j2].s);
        t = (a2 * ss - a3 * sr1 * sr2);
        VSSAdd (w, t, dr, a2, w);
        VVAdd (mol[j1].ra, w);
        VVSub (mol[j2].ra, w);
        VVSAdd (mol[j1].sa, - a1, mol[j2].s);
        VVSAdd (mol[j1].sa, a2 * sr2, dr);
        VVSAdd (mol[j2].sa, - a1, mol[j1].s);
        VVSAdd (mol[j2].sa, a2 * sr1, dr);
        uSum += a1 * ss - a2 * sr1 * sr2;
      }
    }
  }
  uSum -= 2. * dipoleInt * Cube (alpha) * nMol * irPi / 3.;
}

void ComputeForcesDipoleF ()
{
  VecR vc, vn, vs;
  real fMult, gr, gs, gu, pc, ps, sumC, sumS, t, w;
  int n, nvv, nx, ny, nz;

  gu = 2. * M_PI * dipoleInt / Cube (region.x);
  gr = 4. * M_PI * gu / region.x;
  gs = 2. * gu;
  EvalSinCos ();
  w = Sqr (M_PI / (region.x * alpha));
  for (nz = 0; nz <= fSpaceLimit; nz ++) {
    for (ny = - fSpaceLimit; ny <= fSpaceLimit; ny ++) {
      for (nx = - fSpaceLimit; nx <= fSpaceLimit; nx ++) {
        VSet (vn, nx, ny, nz);
        nvv = VLenSq (vn);
        if (nvv == 0 || nvv > Sqr (fSpaceLimit)) continue;
        fMult = 2. * exp (- w * nvv) / nvv;
        if (nz == 0) fMult *= 0.5;
        sumC = sumS = 0.;
        DO_MOL {
          VSet (vc, tCos[abs (nx)][n].x, tCos[abs (ny)][n].y,
             tCos[nz][n].z);
          VSet (vs, tSin[abs (nx)][n].x, tSin[abs (ny)][n].y,
             tSin[nz][n].z);
          if (nx < 0) vs.x = - vs.x;
          if (ny < 0) vs.y = - vs.y;
          pc = vc.x * vc.y * vc.z - vc.x * vs.y * vs.z -
             vs.x * vc.y * vs.z - vs.x * vs.y * vc.z;
          ps = vs.x * vc.y * vc.z + vc.x * vs.y * vc.z +
             vc.x * vc.y * vs.z - vs.x * vs.y * vs.z;
          sumC += VDot (vn, mol[n].s) * pc;
          sumS += VDot (vn, mol[n].s) * ps;
        }
        DO_MOL {
          VSet (vc, tCos[abs (nx)][n].x, tCos[abs (ny)][n].y,
             tCos[nz][n].z);
          VSet (vs, tSin[abs (nx)][n].x, tSin[abs (ny)][n].y,
             tSin[nz][n].z);
          if (nx < 0) vs.x = - vs.x;
          if (ny < 0) vs.y = - vs.y;
          pc = vc.x * vc.y * vc.z - vc.x * vs.y * vs.z -
             vs.x * vc.y * vs.z - vs.x * vs.y * vc.z;
          ps = vs.x * vc.y * vc.z + vc.x * vs.y * vc.z +
             vc.x * vc.y * vs.z - vs.x * vs.y * vs.z;
          t = gr * fMult * VDot (vn, mol[n].s) *
             (sumC * ps - sumS * pc);
          VVSAdd (mol[n].ra, t, vn);
          t = gs * fMult * (sumC * pc + sumS * ps);
          VVSAdd (mol[n].sa, - t, vn);
        }
        uSum += gu * fMult * (Sqr (sumC) + Sqr (sumS));
      }
    }
  }
}

void EvalSinCos ()
{
  VecR t, tt, u, w;
  int j, n;

  VSetAll (t, 2. * M_PI);
  VDiv (t, t, region);
  DO_MOL {
    VMul (tt, t, mol[n].r);
    VSetAll (tCos[0][n], 1.);
    VSetAll (tSin[0][n], 0.);
    VSet (tCos[1][n], cos (tt.x), cos (tt.y), cos (tt.z));
    VSet (tSin[1][n], sin (tt.x), sin (tt.y), sin (tt.z));
    VSCopy (u, 2., tCos[1][n]);
    VMul (tCos[2][n], u, tCos[1][n]);
    VMul (tSin[2][n], u, tSin[1][n]);
    VSetAll (tt, 1.);
    VVSub (tCos[2][n], tt);
    for (j = 3; j <= fSpaceLimit; j ++) {
      VMul (w, u, tCos[j - 1][n]);
      VSub (tCos[j][n], w, tCos[j - 2][n]);
      VMul (w, u, tSin[j - 1][n]);
      VSub (tSin[j][n], w, tSin[j - 2][n]);
    }
  }
}


void ComputeDipoleAccel ()
{
  real t;
  int n;

  DO_MOL {
    t = VDot (mol[n].sa, mol[n].s) + mInert * VLenSq (mol[n].sv);
    VVSAdd (mol[n].sa, - t, mol[n].s);
    VScale (mol[n].sa, 1. / mInert);
  }
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


#define PS(t)                                               \
   PCR4 (mol[n].s, mol[n].s, mol[n].sv,                     \
   mol[n].sa, mol[n].sa1, mol[n].sa2, t)
#define PSV(t)                                              \
   PCV4 (mol[n].s, mol[n].so, mol[n].sv,                    \
   mol[n].sa, mol[n].sa1, mol[n].sa2, t)
#define CS(t)                                               \
   PCR4 (mol[n].s, mol[n].so, mol[n].svo,                   \
   mol[n].sa, mol[n].sa1, mol[n].sa2, t)
#define CSV(t)                                              \
   PCV4 (mol[n].s, mol[n].so, mol[n].sv,                    \
   mol[n].sa, mol[n].sa1, mol[n].sa2, t)

void PredictorStepS ()
{
  real cr[] = {19.,-10.,3.}, cv[] = {27.,-22.,7.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    mol[n].so = mol[n].s;
    mol[n].svo = mol[n].sv;
    PS (x);
    PSV (x);
    PS (y);
    PSV (y);
    PS (z);
    PSV (z);
    mol[n].sa2 = mol[n].sa1;
    mol[n].sa1 = mol[n].sa;
  }
}

void CorrectorStepS ()
{
  real cr[] = {3.,10.,-1.}, cv[] = {7.,6.,-1.}, div = 24., wr, wv;
  int n;

  wr = Sqr (deltaT) / div;
  wv = deltaT / div;
  DO_MOL {
    CS (x);
    CSV (x);
    CS (y);
    CSV (y);
    CS (z);
    CSV (z);
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
  int n;

  VZero (vSum);
  DO_MOL VVAdd (vSum, mol[n].rv);
  DO_MOL VVSAdd (mol[n].rv, - 1. / nMol, vSum);
  vvSum = 0.;
  DO_MOL vvSum += VLenSq (mol[n].rv);
  vFac = velMag / sqrt (vvSum / nMol);
  DO_MOL VScale (mol[n].rv, vFac);
  vvsSum = 0.;
  DO_MOL vvsSum += mInert * VLenSq (mol[n].sv);
  vFac = velMag / sqrt (1.5 * vvsSum / nMol);
  DO_MOL VScale (mol[n].sv, vFac);
}

void ApplyThermostat ()
{
  real s1, s2, vFac;
  int n;

  s1 = s2 = 0.;
  DO_MOL {
    s1 += VDot (mol[n].rv, mol[n].ra);
    s2 += VLenSq (mol[n].rv);
  }
  DO_MOL {
    s1 += mInert * VDot (mol[n].sv, mol[n].sa);
    s2 += mInert * VLenSq (mol[n].sv);
  }
  vFac = - s1 / s2;
  DO_MOL VVSAdd (mol[n].ra, vFac, mol[n].rv);
  DO_MOL VVSAdd (mol[n].sa, vFac, mol[n].sv);
}

void AdjustDipole ()
{
  real sFac;
  int n;

  DO_MOL {
    sFac = VLen (mol[n].s);
    VScale (mol[n].s, 1. / sFac);
  }
}

void EvalRdf ()
{
  VecR dr;
  real deltaR, normFac, rr, sr1, sr2, ss;
  int j1, j2, k, n;

  if (countRdf == 0) {
    for (k = 0; k < 3; k ++) {
      for (n = 0; n < sizeHistRdf; n ++) histRdf[k][n] = 0.;
    }
  }
  deltaR = rangeRdf / sizeHistRdf;
  for (j1 = 0; j1 < nMol - 1; j1 ++) {
    for (j2 = j1 + 1; j2 < nMol; j2 ++) {
      VSub (dr, mol[j1].r, mol[j2].r);
      VWrapAll (dr);
      rr = VLenSq (dr);
      if (rr < Sqr (rangeRdf)) {
        ss = VDot (mol[j1].s, mol[j2].s);
        sr1 = VDot (mol[j1].s, dr);
        sr2 = VDot (mol[j2].s, dr);
        n = sqrt (rr) / deltaR;
        ++ histRdf[0][n];
        histRdf[1][n] += ss;
        histRdf[2][n] += 3. * sr1 * sr2 / rr - ss;
      }
    }
  }
  ++ countRdf;
  if (countRdf == limitRdf) {
    normFac = VProd (region) / (2. * M_PI * Cube (deltaR) *
       Sqr (nMol) * countRdf);
    for (k = 0; k < 3; k ++) {
      for (n = 0; n < sizeHistRdf; n ++)
         histRdf[k][n] *= normFac / Sqr (n - 0.5);
    }
    PrintRdf (stdout);
    countRdf = 0;
  }
}

void PrintRdf (FILE *fp)
{
  real rb;
  int k, n;

  fprintf (fp, "rdf\n");
  for (n = 0; n < sizeHistRdf; n ++) {
    rb = (n + 0.5) * rangeRdf / sizeHistRdf;
    fprintf (fp, "%8.4f", rb);
    for (k = 0; k < 3; k ++) fprintf (fp, " %8.4f", histRdf[k][n]);
    fprintf (fp, "\n");
  }
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
  int n;

  DO_MOL VSet (mol[n].s, 0., 0., 1.);
}

void InitAngVels ()
{
  real ang, angvFac;
  int n;

  angvFac = velMag / sqrt (1.5 * mInert);
  DO_MOL {
    ang = 2. * M_PI * RandR ();
    VSet (mol[n].sv, cos (ang), sin (ang), 0.);
    VScale (mol[n].sv, angvFac);
  }
}

void InitAngAccels ()
{
  int n;

  DO_MOL {
    VZero (mol[n].sa);
    VZero (mol[n].sa1);
    VZero (mol[n].sa2);
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
  vvsSum = 0.;
  DO_MOL vvsSum += mInert * VLenSq (mol[n].sv);
  vvSum += vvsSum;
  kinEnergy.val = 0.5 * vvSum / nMol;
  totEnergy.val = kinEnergy.val + uSum / nMol;
  VZero (w);
  DO_MOL VVAdd (w, mol[n].s);
  dipoleOrder.val = VLen (w) / nMol;
}

void AccumProps (int icode)
{
  if (icode == 0) {
    PropZero (totEnergy);
    PropZero (kinEnergy);
    PropZero (dipoleOrder);
  } else if (icode == 1) {
    PropAccum (totEnergy);
    PropAccum (kinEnergy);
    PropAccum (dipoleOrder);
  } else if (icode == 2) {
    PropAvg (totEnergy, stepAvg);
    PropAvg (kinEnergy, stepAvg);
    PropAvg (dipoleOrder, stepAvg);
  }
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

