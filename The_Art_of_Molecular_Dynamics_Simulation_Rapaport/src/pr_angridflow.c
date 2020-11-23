
/* [[pr_angridflow - process flow snapshots]] */


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


#define NDIM  2

#include "in_mddefs.h"

VecR region;
VecI sizeHistGrid;
real **histGrid, *streamFun, histMax[NHIST], timeNow;
int blockNum, blockSize, plotMode, runId, snapNumber;
char *fName;

enum {P_STREAM, P_ARROW, P_TEMP, P_DENS};

int main (int argc, char **argv)
{
  int n, nData;

  -- argc;
  plotMode = P_STREAM;
  n = 1;
  while (argc >= 1) {
    if      (! strncmp (argv[n], "-a", 2)) plotMode = P_ARROW;
    else if (! strncmp (argv[n], "-t", 2)) plotMode = P_TEMP;
    else if (! strncmp (argv[n], "-d", 2)) plotMode = P_DENS;
    else break;
    ++ n;
    -- argc;
  }
  fName = argv[n];
  blockNum = -1;
  nData = 0;
  while (GetGridAverage ()) {
    ++ nData;
    PutPlotData ();
  }
  exit (0);
}

void PutPlotData ()
{
  VecR w;
  real h, sFirst, sMax, sMin;
  int ix, iy, kp, n;

  printf ("plot_data\n");
  printf ("title: %s %d%s\n", "\"graph", snapNumber, "\"");
  printf ("dim: 2\n");
  printf ("array_size: %d %d\n", sizeHistGrid.x, sizeHistGrid.y);
  printf ("array_order: 1\n");
  printf ("grid_xlimits: %.2f %.2f\n", -0.5 * region.x, 0.5 * region.x);
  printf ("grid_ylimits: %.2f %.2f\n", -0.5 * region.y, 0.5 * region.y);
  if (plotMode == P_ARROW) printf ("vector_field:\n");
  printf ("#\n");
  if (plotMode == P_STREAM) {
    VDiv (w, region, sizeHistGrid);
    sFirst = 0.;
    for (iy = 0; iy < sizeHistGrid.y; iy ++) {
      for (ix = 0; ix < sizeHistGrid.x; ix ++) {
        n = iy * sizeHistGrid.x + ix;
        if (ix == 0) {
          sFirst -= histGrid[0][n] * histGrid[2][n] * w.y;
          streamFun[n] = sFirst;
        } else streamFun[n] = streamFun[n - 1] +
           histGrid[0][n] * histGrid[3][n] * w.x;
      }
    }
    sMin = 1e6;
    sMax = - sMin;
    for (n = 0; n < VProd (sizeHistGrid); n ++) {
      sMin = Min (sMin, streamFun[n]);
      sMax = Max (sMax, streamFun[n]);
    }
    if (sMax == sMin) sMax = sMin + 1.;
    for (n = 0; n < VProd (sizeHistGrid); n ++) {
      if (histGrid[0][n] > 0.)
         streamFun[n] = 999. * (streamFun[n] - sMin) / (sMax - sMin);
      else streamFun[n] = 0;
    }
    for (n = 0; n < VProd (sizeHistGrid); n ++) {
      printf ("%4.0f", streamFun[n]);
      if (n > 0 && n % 50 == 0) printf ("\n");
    }
    if (n - 1 % 50 != 0) printf ("\n");
  } else if (plotMode == P_TEMP || plotMode == P_DENS) {
    sMin = 1e6;
    sMax = -1e6;
    if (plotMode == P_TEMP) kp = 1;
    else if (plotMode == P_DENS) kp = 0;
    for (n = 0; n < VProd (sizeHistGrid); n ++) {
      sMin = Min (sMin, histGrid[kp][n]);
      sMax = Max (sMax, histGrid[kp][n]);
    }
    for (n = 0; n < VProd (sizeHistGrid); n ++) {
      h = 999. * (histGrid[kp][n] - sMin) / (sMax - sMin);
      printf ("%4.0f", h);
      if (n > 0 && n % 50 == 0) printf ("\n");
    }
    if (n - 1 % 50 != 0) printf ("\n");
  } else if (plotMode == P_ARROW) {
    h = 99. / Max (histMax[2], histMax[3]);
    for (n = 0; n < VProd (sizeHistGrid); n ++) {
      printf ("%4d%4d", (int) (h * histGrid[2][n]),
         (int) (h * histGrid[3][n]));
      if (n > 0 && n % 25 == 0) printf ("\n");
    }
    if (n - 1 % 25 != 0) printf ("\n");
  }
  printf ("#\n$\n");
}


#define SCALE_FAC  32767.

int GetGridAverage ()
{
  int fOk, n, j, k;
  short *hI;
  FILE *fp;

  fOk = 1;
  fp = fopen (fName, "r");
  if (blockNum == -1) {
    if (! fp) fOk = 0;
  } else {
    fseek (fp, blockNum * blockSize, 0);
    ++ blockNum;
  }
  if (fOk) {
    ReadF (blockSize);
    ReadF (histMax);
    ReadF (region);
    ReadF (runId);
    ReadF (sizeHistGrid);
    ReadF (snapNumber);
    ReadF (timeNow);
    if (feof (fp)) return (0);
    if (blockNum == -1) {
      AllocMem2 (histGrid, NHIST, VProd (sizeHistGrid), real);
      if (plotMode == P_STREAM) AllocMem (streamFun, VProd (sizeHistGrid), real);
      blockNum = 1;
    }
    AllocMem (hI, VProd (sizeHistGrid), short);
    for (j = 0; j < NHIST; j ++) {
      ReadFN (hI, VProd (sizeHistGrid));
      for (n = 0; n < VProd (sizeHistGrid); n ++)
         histGrid[j][n] = hI[n] * histMax[j] / SCALE_FAC;
    }
    free (hI);
    if (ferror (fp)) fOk = 0;
    fclose (fp);
  }
  if (! fOk) ErrExit (ERR_SNAP_READ);
  return (1);
}


#include "in_errexit.c"

