
/* [[pr_anspcor - space-time correlation analysis]] */


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

#define BUFF_LEN  1024

char *header[] = {"cur-long", "cur-trans", "density"},
   *txtCorr = "space-time corr";

void PrintHelp (char *pName)
{
  printf ("Usage: %s [-t{time_corr}] [-s{skip}n] [-w{window}]"
     "  input-file \n", pName);
  exit (0);
}

int main (int argc, char **argv)
{
  Cmplx *work;
  real *corrSum[3], *corrSumSq[3], damp, deltaT, deltaTCorr,
     omegaMax, tMax, w, x;
  int doFourier, doWindow, j, k, n, nData, nFunCorr, nSet,
     nSetSkip, nv, nValCorr, stepCorr;
  char *bp, *fName, buff[BUFF_LEN];
  FILE *fp;

  n = 1;
  if (-- argc < 1 || ! strcmp (argv[1], "-h")) PrintHelp (argv[0]);
  doFourier = 1;
  doWindow = 0;
  nSetSkip = 1;
  while (-- argc >= 0) {
    if (! strcmp (argv[n], "-t")) doFourier = 0;
    else if (! strcmp (argv[n], "-w")) doWindow = 1;
    else if (! strcmp (argv[n], "-s")) nSetSkip = atoi (argv[n] + 2);
    else {
      fName = argv[n];
      break;
    }
    ++ n;
  }
  if (argc > 0) PrintHelp (argv[0]);
  omegaMax = 10.;
  tMax = 5.;
  if ((fp = fopen (fName, "r")) == 0) {
    printf ("no file\n");
    exit (0);
  }
  while (1) {
    bp = fgets (buff, BUFF_LEN, fp);
    if (*bp == CHAR_MINUS) break;
    NameVal (deltaT);
    NameVal (nFunCorr);
    NameVal (nValCorr);
    NameVal (stepCorr);
  }
  deltaTCorr = stepCorr * deltaT;
  for (j = 0; j < 3; j ++) {
    AllocMem (corrSum[j], nFunCorr * nValCorr, real);
    AllocMem (corrSumSq[j], nFunCorr * nValCorr, real);
    for (n = 0; n < nFunCorr * nValCorr; n ++)
       corrSum[j][n] = corrSumSq[j][n] = 0.;
  }
  AllocMem (work, 2 * (nValCorr - 1), Cmplx);
  nData = 0;
  nSet = 0;
  while (1) {
    if (! (bp = fgets (buff, BUFF_LEN, fp))) break;
    if (! strncmp (bp, txtCorr, strlen (txtCorr))) {
      ++ nSet;
      if (nSet < nSetSkip) continue;
      ++ nData;
      for (j = 0; j < 3; j ++) {
        bp = fgets (buff, BUFF_LEN, fp);
        for (n = 0; n < nValCorr; n ++) {
          bp = fgets (buff, BUFF_LEN, fp);
          w = strtod (bp, &bp);
          for (k = 0; k < nFunCorr; k ++) {
            w = strtod (bp, &bp);
            corrSum[j][k * nValCorr + n] += w;
            corrSumSq[j][k * nValCorr + n] += Sqr (w);
          }
        }
      }
    }
  }
  fclose (fp);
  printf ("%d\n", nData);
  for (j = 0; j < 3; j ++) {
    for (n = 0; n < nFunCorr * nValCorr; n ++) {
      corrSum[j][n] /= nData;
      corrSumSq[j][n] = sqrt (corrSumSq[j][n] / nData - Sqr (corrSum[j][n]));
    }
  }
  if (doFourier) {
    for (j = 0; j < 3; j ++) {
      for (k = 0; k < nFunCorr; k ++) {
        for (n = 0; n < nValCorr; n ++) {
          if (doWindow) damp = (nValCorr - n) / (nValCorr + 0.5);
          else damp = 1.;
          CSet (work[n], corrSum[j][k * nValCorr + n] * damp, 0.);
        }
        for (n = nValCorr; n < 2 * (nValCorr - 1); n ++)
           work[n] = work[2 * (nValCorr - 1) - n];
        FftComplex (work, 2 * nValCorr - 2);
        for (n = 0; n < nValCorr; n ++)
           corrSum[j][k * nValCorr + n] = work[n].R;
      }
    }
    omegaMax = Min (omegaMax, M_PI / deltaTCorr);
    nv = nValCorr * omegaMax / (M_PI / deltaTCorr);
  } else {
    for (j = 0; j < 3; j ++) {
      for (k = 0; k < nFunCorr; k ++) {
        for (n = 1; n < nValCorr; n ++)
           corrSum[j][k * nValCorr + n] /= corrSum[j][k * nValCorr];
        corrSum[j][k * nValCorr] = 1.;
      }
    }
    tMax = Min (tMax, (nValCorr - 1) * deltaTCorr);
    nv = nValCorr * tMax / ((nValCorr - 1) * deltaTCorr);
  }
  for (j = 0; j < 3; j ++) {
    printf ("%s\n", header[j]);
    for (n = 0; n < nv; n ++) {
      if (doFourier) x = n * omegaMax / nv;
      else x = n * deltaTCorr;
      printf ("%9.4f", x);
      for (k = 0; k < nFunCorr; k ++)
         printf (" %9.4f", corrSum[j][k * nValCorr + n]);
      printf ("\n");
    }
  }
}


void FftComplex (Cmplx *a, int size)
{
  Cmplx t, w, wo;
  real theta;
  int i, j, k, n;

  k = 0;
  for (i = 0; i < size; i ++) {
    if (i < k) {
      t = a[i];
      a[i] = a[k];
      a[k] = t;
    }
    n = size / 2;
    while (n >= 1 && k >= n) {
      k -= n;
      n /= 2;
    }
    k += n;
  }
  for (n = 1; n < size; n *= 2) {
    theta = M_PI / n;
    CSet (wo, cos (theta) - 1., sin (theta));
    CSet (w, 1., 0.);
    for (k = 0; k < n; k ++) {
      for (i = k; i < size; i += 2 * n) {
        j = i + n;
        CMul (t, w, a[j]);
        CSub (a[j], a[i], t);
        CAdd (a[i], a[i], t);
      }
      CMul (t, w, wo);
      CAdd (w, w, t);
    }
  }
}


#include "in_errexit.c"

