
/* [[pr_17_1 - distributed processing]] */


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


#define USE_MPI  1

/* for compile: -I/usr/include/openmpi-x86_64 -L/usr/lib64/openmpi/lib -lmpi */

#include "in_mddefs.h"

#include <mpi.h>

#define ME_BOSS   (procMe == 0)

#define MPI_MY_REAL  MPI_DOUBLE

#define MsgStartup()                                        \
   MPI_Init (&argc, &argv),                                 \
   MPI_Comm_size (MPI_COMM_WORLD, &nProc),                  \
   MPI_Comm_rank (MPI_COMM_WORLD, &procMe),                 \
   AllocMem (buffSend, BUFF_LEN, real),                     \
   AllocMem (buffRecv,BUFF_LEN, real)
#define MsgSend(to, id)                                     \
   MPI_Send (buffSend, buffWords, MPI_MY_REAL, to, id,      \
   MPI_COMM_WORLD)
#define MsgRecv(from, id)                                   \
   MPI_Recv (buffRecv, BUFF_LEN, MPI_MY_REAL, from, id,     \
   MPI_COMM_WORLD, &mpiStatus)
#define MsgSendRecv(from, to, id)                           \
   MPI_Sendrecv (buffSend, buffWords, MPI_MY_REAL, to, id,  \
   buffRecv, BUFF_LEN, MPI_MY_REAL, from, id,               \
   MPI_COMM_WORLD, &mpiStatus)
#define MsgBcSend(id)                                       \
   for (mpiNp = 1; mpiNp < nProc; mpiNp ++) MsgSend (mpiNp, id)
#define MsgBcRecv(id)       MsgRecv (0, id)
#define MsgExit()           MPI_Finalize (); exit (0)
#define MsgSendInit()       buffWords = 0
#define MsgRecvInit()       buffWords = 0
#define MsgPackR(v, nv)     DoPackReal (v, nv)
#define MsgPackI(v, nv)     DoPackInt (v, nv)
#define MsgUnpackR(v, nv)   DoUnpackReal (v, nv)
#define MsgUnpackI(v, nv)   DoUnpackInt (v, nv)

#define DO_MOL_ME  for (n = 0; n < nMolMe; n ++)
#define DO_SLAVES  for (np = 1; np < nProc; np ++)

#define MsgRecvUnpack(from, id, ms)                         \
   {MsgRecv (from, id);                                     \
    MsgRecvInit ();                                         \
    UnpackValList (ms, sizeof (ms));}
#define MsgPackSend(to, id, ms)                             \
   {MsgSendInit ();                                         \
    PackValList (ms, sizeof (ms));                          \
    MsgSend (to, id);}
#define MsgBcRecvUnpack(id, ms)                             \
   {MsgBcRecv (id);                                         \
    MsgRecvInit ();                                         \
    UnpackValList (ms, sizeof (ms));}
#define MsgBcPackSend(id, ms)                               \
   if (nProc > 1) {                                         \
     MsgSendInit ();                                        \
     PackValList (ms, sizeof (ms));                         \
     MsgBcSend (id);                                        \
   }

#define BUFF_LEN  64000

MPI_Status mpiStatus;
real *buffRecv, *buffSend;
int buffWords, mpiNp;

typedef struct {
  VecR r, rv, ra;
  int id;
} Mol;

Mol *mol;
VecR region, vSum;
VecI initUcell;
real deltaT, density, rCut, temperature, timeNow, uSum, velMag, vvSum;
Prop kinEnergy, totEnergy;
int moreCycles, nMol, randSeed, stepAvg, stepCount, stepEquil, stepLimit;
VecI cells;
int *cellList;
real dispHi, rNebrShell;
int *nebrTab, nebrNow, nebrTabFac, nebrTabLen, nebrTabMax;
real virSum;
Prop pressure;
VecR subRegionHi, subRegionLo;
real *trBuff;
VecI procArrayMe, procArraySize, procNebrHi, procNebrLo;
int **trPtr, nOut[2][NDIM], errCode, nMolCopy, nMolMe, nMolMeMax,
   nProc, procMe, trBuffMax;

NameList nameList[] = {
  NameR (deltaT),
  NameR (density),
  NameI (initUcell),
  NameI (nMolMeMax),
  NameI (nebrTabFac),
  NameI (procArraySize),
  NameI (randSeed),
  NameR (rNebrShell),
  NameI (stepAvg),
  NameI (stepEquil),
  NameI (stepLimit),
  NameR (temperature),
  NameI (trBuffMax),
};

int main (int argc, char **argv)
{
  MsgStartup ();
  if (ME_BOSS) {
    GetNameList (argc, argv);
    PrintNameList (stdout);
  }
  InitSlaves ();
  NebrParlProcs ();
  SetParams ();
  SetupJob ();
  moreCycles = 1;
  while (moreCycles) {
    SingleStep ();
    if (stepCount == stepLimit) moreCycles = 0;
  }
  MsgExit ();
}

void SetupJob ()
{
  AllocArrays ();
  InitRand (randSeed);
  stepCount = 0;
  InitState ();
  nebrNow = 1;
  if (ME_BOSS) AccumProps (0);
}

void SetParams ()
{
  VecR w;

  rCut = pow (2., 1./6.);
  VSCopy (region, 1. / pow (density, 1./3.), initUcell);
  nMol = VProd (initUcell);
  velMag = sqrt (NDIM * (1. - 1. / nMol) * temperature);
  nebrTabMax = nebrTabFac * nMolMeMax;
  VDiv (w, region, procArraySize);
  VMul (subRegionLo, procArrayMe, w);
  VVSAdd (subRegionLo, -0.5, region);
  VAdd (subRegionHi, subRegionLo, w);
  VScale (w, 1. / (rCut + rNebrShell));
  VAddCon (cells, w, 2.);
}

void AllocArrays ()
{
  int k;

  AllocMem (mol, nMolMeMax, Mol);
  AllocMem (cellList, VProd (cells) + nMolMeMax, int);
  AllocMem (nebrTab, 2 * nebrTabMax, int);
  AllocMem (trBuff, NDIM * trBuffMax, real);
  AllocMem2 (trPtr, 2, NDIM * trBuffMax, int);
}

void SingleStep ()
{
  ValList msg[] = {
    ValI (moreCycles),
    ValI (nebrNow),
  };

  ++ stepCount;
  timeNow = stepCount * deltaT;
  LeapfrogStep (1);
  if (nebrNow > 0) DoParlMove ();
  DoParlCopy ();
  if (nebrNow > 0) {
    nebrNow = 0;
    if (ME_BOSS) dispHi = 0.;
    BuildNebrList ();
  }
  ComputeForces ();
  LeapfrogStep (2);
  EvalProps ();
  if (ME_BOSS) {
    MsgBcPackSend (151, msg);
  } else MsgBcRecvUnpack (151, msg);
  if (ME_BOSS) {
    if (stepCount >= stepEquil) {
      AccumProps (1);
      if (stepCount > stepEquil &&
         (stepCount - stepEquil) % stepAvg == 0) {
        AccumProps (2);
        PrintSummary (stdout);
        AccumProps (0);
      }
    }
  }
}

void PrintSummary (FILE *fp)
{
  fprintf (fp, "%5d %8.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
     stepCount, timeNow, VCSum (vSum) / nMol, PropEst (totEnergy),
     PropEst (kinEnergy));
  fprintf (fp, " %7.4f %7.4f", PropEst (pressure));
  fprintf (fp, "\n");
  fflush (fp);
}

#define OFFSET_LIST                                         \
   { {7}, {7,8,9}, {9}, {6,7,13},                           \
     {5,6,7,8,9,10,11,12,13}, {9,10,11}, {13}, {11,12,13},  \
     {11}, {2,7}, {2,3,4,7,8,9}, {4,9}, {1,2,6,7,13},       \
     {0,1,2,3,4,5,6,7,8,9,10,11,12,13}, {4,9,10,11}, {13},  \
     {11,12,13}, {11}                                       \
   }
#define OFFSET_LEN {1,3,1,3,9,3,1,3,1,2,6,2,5,14,4,1,3,1}

void BuildNebrList ()
{
  VecR cellBase, dr, invWid, rs, t1, t2;
  VecI cc, m1v, m2v, vOff[] = OFFSET_VALS;
  real rrNebr;
  int c, indx, j1, j2, m1, m1x, m1y, m1z, m2, n, offset, tOffset,
     vOffList[][N_OFFSET] = OFFSET_LIST, vOffTableLen[] = OFFSET_LEN;

  VAddCon (t1, cells, -2.);
  VSub (t2, subRegionHi, subRegionLo);
  VDiv (invWid, t1, t2);
  VSetAll (t1, 1.);
  VDiv (t1, t1, invWid);
  VSub (cellBase, subRegionLo, t1);
  rrNebr = Sqr (rCut + rNebrShell);
  for (n = nMolMe + nMolCopy; n < nMolMe + nMolCopy +
     VProd (cells); n ++) cellList[n] = -1;
  for (n = 0; n < nMolMe + nMolCopy; n ++) {
    VSub (rs, mol[n].r, cellBase);
    VMul (cc, rs, invWid);
    c = VLinear (cc, cells) + nMolMe + nMolCopy;
    cellList[n] = cellList[c];
    cellList[c] = n;
  }
  nebrTabLen = 0;
  for (m1z = 0; m1z < cells.z - 1; m1z ++) {
    for (m1y = 0; m1y < cells.y; m1y ++) {
      for (m1x = 0; m1x < cells.x; m1x ++) {
        VSet (m1v, m1x, m1y, m1z);
        tOffset = 13;
        if (m1z == 0) tOffset -= 9;
        if (m1y == 0) tOffset -= 3;
        else if (m1y == cells.y - 1) tOffset += 3;
        if (m1x == 0) tOffset -= 1;
        else if (m1x == cells.x - 1) tOffset += 1;
        m1 = VLinear (m1v, cells) + nMolMe + nMolCopy;
        for (offset = 0; offset < vOffTableLen[tOffset]; offset ++) {
          indx = vOffList[tOffset][offset];
          VAdd (m2v, m1v, vOff[indx]);
          m2 = VLinear (m2v, cells) + nMolMe + nMolCopy;
          DO_CELL (j1, m1) {
            DO_CELL (j2, m2) {
              if (m1 != m2 || j2 < j1) {
                VSub (dr, mol[j1].r, mol[j2].r);
                if (VLenSq (dr) < rrNebr) {
                  nebrTab[2 * nebrTabLen] = j1;
                  nebrTab[2 * nebrTabLen + 1] = j2;
                  ++ nebrTabLen;
                  if (nebrTabLen >= nebrTabMax) {
                    errCode = ERR_TOO_MANY_NEBRS;
                    -- nebrTabLen;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

void ComputeForces ()
{
  VecR dr;
  real fcVal, rr, rrCut, rri, rri3, uVal;
  int j1, j2, n;

  rrCut = Sqr (rCut);
  for (n = 0; n < nMolMe + nMolCopy; n ++) VZero (mol[n].ra);
  uSum = 0.;
  virSum = 0.;
  for (n = 0; n < nebrTabLen; n ++) {
    j1 = nebrTab[2 * n];
    j2 = nebrTab[2 * n + 1];
    VSub (dr, mol[j1].r, mol[j2].r);
    rr = VLenSq (dr);
    if (rr < rrCut) {
      rri = 1. / rr;
      rri3 = Cube (rri);
      fcVal = 48. * rri3 * (rri3 - 0.5) * rri;
      uVal = 4. * rri3 * (rri3 - 1.) + 1.;
      VVSAdd (mol[j1].ra, fcVal, dr);
      VVSAdd (mol[j2].ra, - fcVal, dr);
      if (j1 < nMolMe) {
        uSum += uVal;
        virSum += fcVal * rr;
      }
      if (j2 < nMolMe) {
        uSum += uVal;
        virSum += fcVal * rr;
      }
    }
  }
  uSum *= 0.5;
  virSum *= 0.5;
}

void LeapfrogStep (int part)
{
  int n;

  if (part == 1) {
    DO_MOL_ME {
      VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
      VVSAdd (mol[n].r, deltaT, mol[n].rv);
    }
  } else {
    DO_MOL_ME VVSAdd (mol[n].rv, 0.5 * deltaT, mol[n].ra);
  }
}

void InitState ()
{
  VecR vSumL;
  VecR c, gap;
  int n, np, nx, ny, nz;
  ValList msg1[] = {
    ValR (vSumL),
  };
  ValList msg2[] = {
    ValR (vSum),
  };

  VDiv (gap, region, initUcell);
  VZero (vSumL);
  nMol = 0;
  nMolMe = 0;
  for (nz = 0; nz < initUcell.z; nz ++) {
    for (ny = 0; ny < initUcell.y; ny ++) {
      for (nx = 0; nx < initUcell.x; nx ++) {
        VSet (c, nx + 0.25, ny + 0.25, nz + 0.25);
        VMul (c, c, gap);
        VVSAdd (c, -0.5, region);
        VRand (&mol[nMolMe].rv);
        if (VGe (c, subRegionLo) && VLt (c, subRegionHi)) {
          mol[nMolMe].r = c;
          VScale (mol[nMolMe].rv, velMag);
          VVAdd (vSumL, mol[nMolMe].rv);
          mol[nMolMe].id = nMol;
          ++ nMolMe;
          if (nMolMe > nMolMeMax) {
            errCode = ERR_TOO_MANY_MOLS;
            -- nMolMe;
          }
        }
        ++ nMol;
      }
    }
  }
  if (ME_BOSS) {
    vSum = vSumL;
    DO_SLAVES {
      MsgRecvUnpack (np, 121, msg1);
      VVAdd (vSum, vSumL);
    }
    VScale (vSum, 1. / nMol);
    MsgBcPackSend (122, msg2);
  } else {
    MsgPackSend (0, 121, msg1);
    MsgBcRecvUnpack (122, msg2);
  }
  DO_MOL_ME {
    VVSub (mol[n].rv, vSum);
    VZero (mol[n].ra);
  }
}

void EvalProps ()
{
  VecR vSumL;
  real uSumL, virSumL, vvMaxL, vvSumL;
  int errCodeL;
  real vv, vvMax;
  int n, np;
  ValList msg[] = {
    ValI (errCodeL),
    ValR (uSumL),
    ValR (virSumL),
    ValR (vSumL),
    ValR (vvMaxL),
    ValR (vvSumL),
  };

  VZero (vSumL);
  vvSumL = 0.;
  vvMaxL = 0.;
  DO_MOL_ME {
    VVAdd (vSumL, mol[n].rv);
    vv = VLenSq (mol[n].rv);
    vvSumL += vv;
    vvMaxL = Max (vvMaxL, vv);
  }
  if (ME_BOSS) {
    vSum = vSumL;
    vvSum = vvSumL;
    vvMax = vvMaxL;
    DO_SLAVES {
      MsgRecvUnpack (np, 161, msg);
      if (errCodeL != ERR_NONE) errCode = errCodeL;
      vvMax = Max (vvMax, vvMaxL);
      vvSum += vvSumL;
      VVAdd (vSum, vSumL);
      uSum += uSumL;
      virSum += virSumL;
    }
    dispHi += sqrt (vvMax) * deltaT;
    if (dispHi > 0.5 * rNebrShell) nebrNow = 1;
    kinEnergy.val = 0.5 * vvSum / nMol;
    totEnergy.val = kinEnergy.val + uSum / nMol;
    pressure.val = density * (vvSum + virSum) / (nMol * NDIM);
  } else {
    errCodeL = errCode;
    uSumL = uSum;
    virSumL = virSum;
    MsgPackSend (0, 161, msg);
  }
}


void AccumProps (int icode)
{
  if (icode == 0) {
    PropZero (totEnergy);
    PropZero (kinEnergy);
    PropZero (pressure);
  } else if (icode == 1) {
    PropAccum (totEnergy);
    PropAccum (kinEnergy);
    PropAccum (pressure);
  } else if (icode == 2) {
    PropAvg (totEnergy, stepAvg);
    PropAvg (kinEnergy, stepAvg);
    PropAvg (pressure, stepAvg);
  }
}


void InitSlaves ()
{
  ValList initVals[] = {
    ValR (deltaT),
    ValR (density),
    ValI (initUcell),
    ValI (nebrTabFac),
    ValI (nMolMeMax),
    ValI (procArraySize),
    ValI (randSeed),
    ValR (rNebrShell),
    ValI (stepEquil),
    ValI (stepLimit),
    ValR (temperature),
    ValI (trBuffMax),
  };

  if (ME_BOSS) {
    MsgBcPackSend (111, initVals);
  } else MsgBcRecvUnpack (111, initVals);
}

void NebrParlProcs ()
{
  VecI t;
  int k;

  nProc = VProd (procArraySize);
  procArrayMe.x = procMe % procArraySize.x;
#if NDIM == 2
  procArrayMe.y = procMe / procArraySize.x;
#endif
#if NDIM == 3
  procArrayMe.y = (procMe / procArraySize.x) % procArraySize.y;
  procArrayMe.z = procMe / (procArraySize.x * procArraySize.y);
#endif
  for (k = 0; k < NDIM; k ++) {
    t = procArrayMe;
    VComp (t, k) = (VComp (t, k) + VComp (procArraySize, k) - 1) %
       VComp (procArraySize, k);
    VComp (procNebrLo, k) = VLinear (t, procArraySize);
    t = procArrayMe;
    VComp (t, k) = (VComp (t, k) + 1) % VComp (procArraySize, k);
    VComp (procNebrHi, k) = VLinear (t, procArraySize);
  }
}

#define OutsideProc(b)                                      \
   (sDir == 0 && VComp (mol[n].r, dir) <                    \
   VComp (subRegionLo, dir) + b ||                          \
   sDir == 1 && VComp (mol[n].r, dir) >=                    \
   VComp (subRegionHi, dir) - b)

#define NWORD_COPY  (NDIM + 1)

void DoParlCopy ()
{
  real rCutExt;
  int dir, n, nIn, nt, sDir;

  rCutExt = rCut + rNebrShell;
  nMolCopy = 0;
  for (dir = 0; dir < NDIM; dir ++) {
    if (nebrNow > 0) {
      for (sDir = 0; sDir < 2; sDir ++) {
        nt = 0;
        for (n = 0; n < nMolMe + nMolCopy; n ++) {
          if (OutsideProc (rCutExt)) {
            trPtr[sDir][trBuffMax * dir + nt] = n;
            ++ nt;
            if (NWORD_COPY * nt > NDIM * trBuffMax) {
              errCode = ERR_COPY_BUFF_FULL;
              -- nt;
            }
          }
        }
        nOut[sDir][dir] = nt;
      }
    }
    for (sDir = 0; sDir < 2; sDir ++) {
      nt = nOut[sDir][dir];
      PackCopiedData (dir, sDir, &trPtr[sDir][trBuffMax * dir], nt);
      if (VComp (procArraySize, dir) > 1) {
        MsgSendInit ();
        MsgPackI (&nt, 1);
        MsgPackR (trBuff, NWORD_COPY * nt);
        if (sDir == 1) MsgSendRecv (VComp (procNebrLo, dir),
           VComp (procNebrHi, dir), 130 + 2 * dir + 1);
        else MsgSendRecv (VComp (procNebrHi, dir),
           VComp (procNebrLo, dir), 130 + 2 * dir);
        MsgRecvInit ();
        MsgUnpackI (&nIn, 1);
        MsgUnpackR (trBuff, NWORD_COPY * nIn);
      } else nIn = nt;
      if (nMolMe + nMolCopy + nIn > nMolMeMax) {
        errCode = ERR_TOO_MANY_COPIES;
        nIn = 0;
      }
      UnpackCopiedData (nIn);
    }
  }
}

void PackCopiedData (int dir, int sDir, int *trPtr, int nt)
{
  real rShift;
  int j;

  rShift = 0.;
  if (sDir == 1 &&
     VComp (procArrayMe, dir) == VComp (procArraySize, dir) - 1)
     rShift = - VComp (region, dir);
  else if (sDir == 0 &&
     VComp (procArrayMe, dir) == 0) rShift = VComp (region, dir);
  for (j = 0; j < nt; j ++) {
    VToLin (trBuff, NWORD_COPY * j, mol[trPtr[j]].r);
    trBuff[NWORD_COPY * j + dir] += rShift;
    trBuff[NWORD_COPY * j + NDIM] = mol[trPtr[j]].id;
  }
}

void UnpackCopiedData (int nIn)
{
  int j;

  for (j = 0; j < nIn; j ++) {
    VFromLin (mol[nMolMe + nMolCopy + j].r, trBuff, NWORD_COPY * j);
    mol[nMolMe + nMolCopy + j].id = trBuff[NWORD_COPY * j + NDIM];
  }
  nMolCopy += nIn;
}

#define NWORD_MOVE  (2 * NDIM + 1)

void DoParlMove ()
{
  int dir, n, nIn, nt, sDir;

  for (dir = 0; dir < NDIM; dir ++) {
    for (sDir = 0; sDir < 2; sDir ++) {
      nt = 0;
      DO_MOL_ME {
        if (mol[n].id >= 0) {
          if (OutsideProc (0.)) {
            trPtr[sDir][trBuffMax * dir + nt] = n;
            ++ nt;
            if (NWORD_MOVE * nt > NDIM * trBuffMax) {
              errCode = ERR_COPY_BUFF_FULL;
              -- nt;
            }
          }
        }
      }
      nOut[sDir][dir] = nt;
    }
    for (sDir = 0; sDir < 2; sDir ++) {
      nt = nOut[sDir][dir];
      PackMovedData (dir, sDir, &trPtr[sDir][trBuffMax * dir], nt);
      if (VComp (procArraySize, dir) > 1) {
        MsgSendInit ();
        MsgPackI (&nt, 1);
        MsgPackR (trBuff, NWORD_MOVE * nt);
        if (sDir == 1) MsgSendRecv (VComp (procNebrLo, dir),
           VComp (procNebrHi, dir), 140 + 2 * dir + 1);
        else MsgSendRecv (VComp (procNebrHi, dir),
           VComp (procNebrLo, dir), 140 + 2 * dir);
        MsgRecvInit ();
        MsgUnpackI (&nIn, 1);
        MsgUnpackR (trBuff, NWORD_MOVE * nIn);
      } else nIn = nt;
      if (nMolMe + nIn > nMolMeMax) {
        errCode = ERR_TOO_MANY_MOVES;
        nIn = 0;
      }
      UnpackMovedData (nIn);
    }
  }
  RepackMolArray ();
}

void PackMovedData (int dir, int sDir, int *trPtr, int nt)
{
  real rShift;
  int j;

  rShift = 0.;
  if (sDir == 1 &&
     VComp (procArrayMe, dir) == VComp (procArraySize, dir) - 1)
     rShift = - VComp (region, dir);
  else if (sDir == 0 &&
     VComp (procArrayMe, dir) == 0) rShift = VComp (region, dir);
  for (j = 0; j < nt; j ++) {
    VToLin (trBuff, NWORD_MOVE * j, mol[trPtr[j]].r);
    trBuff[NWORD_MOVE * j + dir] += rShift;
    VToLin (trBuff, NWORD_MOVE * j + NDIM, mol[trPtr[j]].rv);
    trBuff[NWORD_MOVE * j + 2 * NDIM] = mol[trPtr[j]].id;
    mol[trPtr[j]].id = -1;
  }
}

void UnpackMovedData (int nIn)
{
  int j;

  for (j = 0; j < nIn; j ++) {
    VFromLin (mol[nMolMe + j].r, trBuff, NWORD_MOVE * j);
    VFromLin (mol[nMolMe + j].rv, trBuff, NWORD_MOVE * j + NDIM);
    mol[nMolMe + j].id = trBuff[NWORD_MOVE * j + 2 * NDIM];
  }
  nMolMe += nIn;
}

void RepackMolArray ()
{
  int j, n;

  j = 0;
  DO_MOL_ME {
    if (mol[n].id >= 0) {
      mol[j] = mol[n];
      ++ j;
    }
  }
  nMolMe = j;
}

void DoPackReal (real *w, int nw)
{
  int n;

  if (buffWords + nw >= BUFF_LEN) {
    errCode = ERR_MSG_BUFF_FULL;
  } else {
    for (n = 0; n < nw; n ++) buffSend[buffWords + n] = w[n];
    buffWords += nw;
  }
}

void DoPackInt (int *w, int nw)
{
  int n;

  if (buffWords + nw >= BUFF_LEN) {
    errCode = ERR_MSG_BUFF_FULL;
  } else {
    for (n = 0; n < nw; n ++) buffSend[buffWords + n] = (real) w[n];
    buffWords += nw;
  }
}

void DoUnpackReal (real *w, int nw)
{
  int n;

  for (n = 0; n < nw; n ++) w[n] = buffRecv[buffWords + n];
  buffWords += nw;
}

void DoUnpackInt (int *w, int nw)
{
  int n;

  for (n = 0; n < nw; n ++) w[n] = (int) buffRecv[buffWords + n];
  buffWords += nw;
}

void PackValList (ValList *list, int size)
{
  int k;

  for (k = 0; k < size / sizeof (ValList); k ++) {
    switch (list[k].vType) {
      case N_I:
        MsgPackI (list[k].vPtr, list[k].vLen);
        break;
      case N_R:
        MsgPackR (list[k].vPtr, list[k].vLen);
        break;
    }
  }
}

void UnpackValList (ValList *list, int size)
{
  int k;

  for (k = 0; k < size / sizeof (ValList); k ++) {
    switch (list[k].vType) {
      case N_I:
        MsgUnpackI (list[k].vPtr, list[k].vLen);
        break;
      case N_R:
        MsgUnpackR (list[k].vPtr, list[k].vLen);
        break;
    }
  }
}

#include "in_rand.c"
#include "in_errexit.c"
#include "in_namelist.c"

