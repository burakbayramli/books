
/* [[pr_ewaldtest - test Ewald sums]] */


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
  VecR r, ra;
  VecR s, sa;
} Mol;

Mol *mol;
VecR **tCos, **tSin, region;
VecI initUcell;
real alpha, dipoleInt, raSum, saSum, uSum;
int fSpaceLimit, nMol, nFmax, randSeed;

int main ()
{
  real t;
  int k, n, na, nf;

  dipoleInt = 1.;
  randSeed = 17;
  VSetAll (region, 10.);
  VSetAll (initUcell, 6);
  nMol = VProd (initUcell);
  nFmax = 7;
  AllocArrays ();
  InitCoords ();
  PerturbCoords ();
  InitAngCoords ();
  for (nf = 4; nf < nFmax; nf ++) {
    for (na = 0; na < 11; na ++) {
      alpha = (4. + 0.25 * na) / region.x;
      fSpaceLimit = nf + 1;
      DO_MOL VZero (mol[n].ra);
      uSum = 0.;
      ComputeForcesDipoleR ();
      ComputeForcesDipoleF ();
      DO_MOL {
        t = VDot (mol[n].sa, mol[n].s);
        VVSAdd (mol[n].sa, - t, mol[n].s);
      }
      raSum = 0.;
      saSum = 0.;
      DO_MOL {
        for (k = 0; k < NDIM; k ++) {
          raSum += fabs (VComp (mol[n].ra, k));
          saSum += fabs (VComp (mol[n].sa, k));
        }
      }
      printf ("%d  %.3f  %.5f  %.5f  %.5f\n",
         fSpaceLimit, alpha * region.x, uSum, raSum, saSum);
    }
    printf ("\n");
  }
}

void AllocArrays ()
{
  int k;

  AllocMem (mol, nMol, Mol);
  AllocMem2 (tCos, nFmax + 1, nMol, VecR);
  AllocMem2 (tSin, nFmax + 1, nMol, VecR);
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


void PerturbCoords ()
{
  VecR gap;
  int k, n;

  VDiv (gap, region, initUcell);
  DO_MOL {
    for (k = 0; k < NDIM; k ++)
       VComp (mol[n].r, k) += (0.5 * RandR () - 0.25) * VComp (gap, k);
  }
}

void InitAngCoords ()
{
  int n;

  DO_MOL VRand (&mol[n].s);
}

#include "in_rand.c"

