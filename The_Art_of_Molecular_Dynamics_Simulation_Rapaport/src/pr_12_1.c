
/* [[pr_12_1 - silicon, RDF]] */


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
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepEquil, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
int *nebrTabPtr;
int stepAdjustTemp;
real *histRdf, rangeRdf;
int countRdf, limitRdf, sizeHistRdf, stepRdf;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (limitRdf),
  NameI (nebrTabFac),
  NameR (rangeRdf),
  NameR (rCut),
  NameR (rNebrShell),
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
  if (nebrNow) {
    nebrNow = 0;
    dispHi = 0.;
    BuildNebrList ();
  }
  PredictorStep ();
  ComputeForces ();
  ApplyThermostat ();
  CorrectorStep ();
  ApplyBoundaryCond ();
  EvalProps ();
  if (stepCount % stepAdjustTemp == 0) AdjustTemp ();
  AccumProps (1);
  if (stepCount % stepAvg == 0) {
    AccumProps (2);
    PrintSummary (stdout);
    AccumProps (0);
  }
  if (stepCount >= stepEquil && (stepCount - stepEquil) %
     stepRdf == 0) EvalRdf ();
}

void SetupJob ()
{
  AllocArrays ();
  stepCount = 0;
  InitCoords ();
  InitVels ();
  InitAccels ();
  AccumProps (0);
  nebrNow = 1;
  countRdf = 0;
}

void SetParams ()
{
  VSCopy (region, 1. / pow (density / 8., 1./3.), initUcell);
  nMol = 8 * VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / (rCut + rNebrShell), region);
  nebrTabMax = nebrTabFac * nMol;
}

void AllocArrays ()
{
  AllocMem (mol, nMol, Mol);
  AllocMem (cellList, VProd (cells) + nMol, int);
  AllocMem (nebrTab, nebrTabMax, int);
  AllocMem (nebrTabPtr, nMol + 1, int);
  AllocMem (histRdf, sizeHistRdf, real);
}

#undef OFFSET_VALS

#define OFFSET_VALS                                       \
   { {-1,-1,-1}, {0,-1,-1}, {1,-1,-1},                    \
     {-1,0,-1}, {0,0,-1}, {1,0,-1}, {-1,1,-1}, {0,1,-1},  \
     {1,1,-1}, {-1,-1,0}, {0,-1,0}, {1,-1,0}, {-1,0,0},   \
     {0,0,0}, {1,0,0}, {-1,1,0}, {0,1,0}, {1,1,0},        \
     {-1,-1,1}, {0,-1,1}, {1,-1,1}, {-1,0,1},  {0,0,1},   \
     {1,0,1}, {-1,1,1}, {0,1,1}, {1,1,1}                  \
   }

void BuildNebrList ()
{
  VecR dr, invWid, rs, shift;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrNebr;
  int c, j1, j2, m2, n, offset;

  rrNebr = Sqr (rCut + rNebrShell);
  VDiv (invWid, cells, region);
  for (n = nMol; n < nMol + VProd (cells); n ++)
     cellList[n] = -1;
  DO_MOL {
    VSAdd (rs, mol[n].r, 0.5, region);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nMol;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
  nebrTabLen = 0;
  for (j1 = 0; j1 < nMol; j1 ++) {
    VSAdd (rs, mol[j1].r, 0.5, region);
    VMul (m1v, rs, invWid);
    nebrTabPtr[j1] = nebrTabLen;
    for (offset = 0; offset < 27; offset ++) {
      VAdd (m2v, m1v, vOff[offset]);
      VZero (shift);
      VCellWrapAll ();
      m2 = VLinear (m2v, cells) + nMol;
      DO_CELL (j2, m2) {
        if (j2 != j1) {
          VSub (dr, mol[j1].r, mol[j2].r);
          VVSub (dr, shift);
          if (VLenSq (dr) < rrNebr) {
            if (nebrTabLen >= nebrTabMax) ErrExit (ERR_TOO_MANY_NEBRS);
            nebrTab[nebrTabLen] = j2;
            ++ nebrTabLen;
          }
        }
      }
    }
  }
  nebrTabPtr[nMol] = nebrTabLen;
}

void ComputeForces ()
{
  VecR dr, dr12, dr13, w2, w3;
  real aCon = 7.0496, bCon = 0.60222, cr, er, fcVal,
     gCon = 1.2, lCon = 21., p12, p13, ri, ri3, rm,
     rm12, rm13, rr, rr12, rr13, rrCut;
  int j1, j2, j3, m2, m3, n;

  rrCut = Sqr (rCut) - 0.001;
  DO_MOL VZero (mol[n].ra);
  uSum = 0.;
  for (j1 = 0; j1 < nMol; j1 ++) {
    for (m2 = nebrTabPtr[j1]; m2 < nebrTabPtr[j1 + 1]; m2 ++) {
      j2 = nebrTab[m2];
      if (j1 < j2) {
        VSub (dr, mol[j1].r, mol[j2].r);
        VWrapAll (dr);
        rr = VLenSq (dr);
        if (rr < rrCut) {
          rm = sqrt (rr);
          er = exp (1. / (rm - rCut));
          ri = 1. / rm;
          ri3 = Cube (ri);
          fcVal = aCon * (4. * bCon * Sqr (ri3) +
             (bCon * ri3 * ri - 1.) * ri / Sqr (rm - rCut)) * er;
          VVSAdd (mol[j1].ra, fcVal, dr);
          VVSAdd (mol[j2].ra, - fcVal, dr);
          uSum += aCon * (bCon * ri3 * ri - 1.) * er;
        }
      }
    }
  }
  for (j1 = 0; j1 < nMol; j1 ++) {
    for (m2 = nebrTabPtr[j1]; m2 < nebrTabPtr[j1 + 1] - 1; m2 ++) {
      j2 = nebrTab[m2];
      VSub (dr12, mol[j1].r, mol[j2].r);
      VWrapAll (dr12);
      rr12 = VLenSq (dr12);
      if (rr12 < rrCut) {
        rm12 = sqrt (rr12);
        VScale (dr12, 1. / rm12);
        for (m3 = m2 + 1; m3 < nebrTabPtr[j1 + 1]; m3 ++) {
          j3 = nebrTab[m3];
          VSub (dr13, mol[j1].r, mol[j3].r);
          VWrapAll (dr13);
          rr13 = VLenSq (dr13);
          if (rr13 < rrCut) {
            rm13 = sqrt (rr13);
            VScale (dr13, 1. / rm13);
            cr = VDot (dr12, dr13);
            er = lCon * (cr + 1./3.) * exp (gCon / (rm12 - rCut) + gCon /
               (rm13 - rCut));
            p12 = gCon * (cr + 1./3.) / Sqr (rm12 - rCut);
            p13 = gCon * (cr + 1./3.) / Sqr (rm13 - rCut);
            VSSAdd (w2, p12 + 2. * cr / rm12, dr12, - 2. / rm12, dr13);
            VSSAdd (w3, p13 + 2. * cr / rm13, dr13, - 2. / rm13, dr12);
            VScale (w2, - er);
            VScale (w3, - er);
            VVSub (mol[j1].ra, w2);
            VVSub (mol[j1].ra, w3);
            VVAdd (mol[j2].ra, w2);
            VVAdd (mol[j3].ra, w3);
            uSum += (cr + 1./3.) * er;
          }
        }
      }
    }
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


void ApplyBoundaryCond ()
{
  int n;

  DO_MOL VWrapAll (mol[n].r);
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


void ApplyThermostat ()
{
  real s1, s2, vFac;
  int n;

  s1 = s2 = 0.;
  DO_MOL {
    s1 += VDot (mol[n].rv, mol[n].ra);
    s2 += VLenSq (mol[n].rv);
  }
  vFac = - s1 / s2;
  DO_MOL VVSAdd (mol[n].ra, vFac, mol[n].rv);
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


void InitCoords ()
{
  VecR c, gap;
  real subShift;
  int j, m, n, nx, ny, nz;

  VDiv (gap, region, initUcell);
  n = 0;
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.125, ny + 0.125, nz + 0.125);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, region);
        for (m = 0; m < 2; m ++) {
          subShift = (m == 1) ? 0.25 : 0.;
          for (j = 0; j < 4; j ++) {
            VSAdd (mol[n].r, c, subShift, gap);
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

