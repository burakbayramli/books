
/* [[pr_17_3 - vector processing]] */


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
  real u;
  int inCell;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, stepAvg, stepCount, stepLimit;
VecI cells;
VecR *raL;
real *uL;
int **layerMol, **molPtr, *inside, *molId, bdyOffset, nLayerMax, nMolMax,
   nMolRep;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (nLayerMax),
  NameI (stepAvg),
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
  ApplyBoundaryCond ();
  ReplicateMols ();
  ComputeForces ();
  LeapfrogStep (2);
  EvalProps ();
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
  InitCoords ();
  InitVels ();
  InitAccels ();
  SetupLayers ();
  AccumProps (0);
}

void SetParams ()
{
  VecI t;

  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density / 4., 1./3.), initUcell);
  nMol = 4 * VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  VSCopy (cells, 1. / rCut, region);
  VAddCon (cells, cells, 2);
  t = initUcell;
  VAddCon (t, t, 2);
  nMolMax = 4 * VProd (t);
  bdyOffset = cells.x * (cells.y + 1) + 1;
}

void AllocArrays ()
{
  int k;

  AllocMem (mol, nMolMax, Mol);
  AllocMem (molId, nMolMax, int);
  AllocMem (raL, VProd (cells), VecR);
  AllocMem (uL, VProd (cells), real);
  AllocMem (inside, 2 * bdyOffset + VProd (cells), int);
  AllocMem2 (molPtr, 2, VProd (cells), int);
  AllocMem2 (layerMol, nLayerMax, 2 * bdyOffset + VProd (cells), int);
}

void ReplicateMols ()
{
  int k, n, na;

  nMolRep = nMol;
  for (k = 0; k < NDIM; k ++) {
    na = nMolRep;
    for (n = 0; n < nMolRep; n ++) {
      if (fabs (VComp (mol[n].r, k)) >= 0.5 * VComp (region, k) - rCut) {
        mol[na].r = mol[n].r;
        if (VComp (mol[na].r, k) > 0.) VComp (mol[na].r, k) -= VComp (region, k);
        else VComp (mol[na].r, k) += VComp (region, k);
        ++ na;
      }
    }
    nMolRep = na;
    if (nMolRep >= nMolMax) ErrExit (ERR_TOO_MANY_REPLICAS);
  }
}

#undef OFFSET_VALS

#define OFFSET_VALS                                             \
   { {-1,-1,-1}, {0,-1,-1}, {1,-1,-1},                          \
     {-1,0,-1}, {0,0,-1}, {1,0,-1}, {-1,1,-1}, {0,1,-1},        \
     {1,1,-1}, {-1,-1,0}, {0,-1,0}, {1,-1,0}, {-1,0,0},         \
     {0,0,0}, {1,0,0}, {-1,1,0}, {0,1,0}, {1,1,0}, {-1,-1,1},   \
     {0,-1,1}, {1,-1,1}, {-1,0,1}, {0,0,1}, {1,0,1}, {-1,1,1},  \
     {0,1,1}, {1,1,1}                                           \
   }

void ComputeForces ()
{
  VecR dr, invWid, regionEx, rs, t;
  VecI cc, vOff[] = OFFSET_VALS;
  real fcVal, rr, rrCut, rri, rri3;
  int ic, layer1, layer2, m1, m2, n, na, nat, nLayer, nPair, offset,
     offsetLo;

  VCopy (t, cells);
  VAddCon (t, t, -2.);
  VDiv (invWid, t, region);
  VSetAll (t, 2.);
  VDiv (t, t, invWid);
  VAdd (regionEx, region, t);
  for (n = 0; n < nMolRep; n ++) {
    VSAdd (rs, mol[n].r, 0.5, regionEx);
    VMul (cc, rs, invWid);
    mol[n].inCell = VLinear (cc, cells);
    molId[n] = n;
  }
  nLayer = 0;
  for (na = nMolRep; na > 0; na = nat) {
    if (nLayer >= nLayerMax) ErrExit (ERR_TOO_MANY_LAYERS);
    for (ic = 0; ic < VProd (cells); ic ++)
       layerMol[nLayer][bdyOffset + ic] = -1;
    for (n = 0; n < na; n ++)
       layerMol[nLayer][bdyOffset + mol[molId[n]].inCell] = molId[n];
    for (ic = 0; ic < VProd (cells); ic ++) {
      n = layerMol[nLayer][bdyOffset + ic];
      if (n >= 0) mol[n].inCell = -1;
    }
    nat = 0;
    for (n = 0; n < na; n ++) {
      if (mol[molId[n]].inCell >= 0) molId[nat ++] = molId[n];
    }
    ++ nLayer;
  }
  for (n = 0; n < nMolRep; n ++) {
    VZero (mol[n].ra);
    mol[n].u = 0.;
  }
  rrCut = Sqr (rCut);
  for (layer1 = 0; layer1 < nLayer; layer1 ++) {
    for (layer2 = layer1; layer2 < nLayer; layer2 ++) {
      offsetLo = (layer2 == layer1) ? 14 : 0;
      for (offset = offsetLo; offset < 27; offset ++) {
        nPair = 0;
        m2 = bdyOffset + VLinear (vOff[offset], cells);
        for (m1 = bdyOffset; m1 < bdyOffset + VProd (cells); m1 ++) {
          if ((inside[m1] || inside[m2]) &&
             layerMol[layer1][m1] >= 0 && layerMol[layer2][m2] >= 0) {
            molPtr[0][nPair] = layerMol[layer1][m1];
            molPtr[1][nPair] = layerMol[layer2][m2];
            ++ nPair;
          }
          ++ m2;
        }
        for (n = 0; n < nPair; n ++) {
          VSub (dr, mol[molPtr[0][n]].r, mol[molPtr[1][n]].r);
          rr = VLenSq (dr);
          if (rr < rrCut) {
            rri = 1. / rr;
            rri3 = Cube (rri);
            fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
            VSCopy (raL[n], fcVal, dr);
            uL[n] = 4. * rri3 * (rri3 - 1.) + 1.;
          } else {
            VZero (raL[n]);
            uL[n] = 0.;
          }
        }
        for (n = 0; n < nPair; n ++) {
          VVAdd (mol[molPtr[0][n]].ra, raL[n]);
          mol[molPtr[0][n]].u += uL[n];
        }
        for (n = 0; n < nPair; n ++) {
          VVSub (mol[molPtr[1][n]].ra, raL[n]);
          mol[molPtr[1][n]].u += uL[n];
        }
      }
    }
  }
  uSum = 0.;
  DO_MOL uSum += mol[n].u;
  uSum *= 0.5;
}

void SetupLayers ()
{
  int n, nLayer, nx, ny, nz;

  for (n = 0; n < 2 * bdyOffset + VProd (cells); n ++) inside[n] = 0;
  for (nz = 1; nz < cells.z - 1; nz ++) {
    for (ny = 1; ny < cells.y - 1; ny ++) {
      for (nx = 1; nx < cells.x - 1; nx ++) {
        inside[bdyOffset + (nz * cells.y + ny) * cells.x + nx] = 1;
      }
    }
  }
  for (nLayer = 0; nLayer < nLayerMax; nLayer ++) {
    for (n = 0; n < 2 * bdyOffset + VProd (cells); n ++)
       layerMol[nLayer][n] = -1;
  }
}


void LeapfrogStep (int part)
{
  int n;

  if (part == 1) {
    DO_MOL {
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
      VVSAdd (mol[n].r, deltaT, mol[n].rv);
    }
  } else {
    DO_MOL VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
  }
}


void ApplyBoundaryCond ()
{
  int n;

  DO_MOL VWrapAll (mol[n].r);
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

  DO_MOL VZero (mol[n].ra);
}


void EvalProps ()
{
  real vv, vvMax;
  int n;

  VZero (vSum);
  vvSum = 0.;
  DO_MOL {
    VVAdd (vSum, mol[n].rv);
    vv = VLenSq (mol[n].rv);
    vvSum += vv;
  }
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

