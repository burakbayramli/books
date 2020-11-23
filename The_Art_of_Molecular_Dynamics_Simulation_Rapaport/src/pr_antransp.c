
/* [[pr_antransp - transport coefficient analysis]] */


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

char *txtAcf = "acf", *txtintAcf = {"acf integrals:"};

int main (int argc, char **argv)
{
  real **sumAcf, **sumAcfSq, *valAcf[3], intAcfSum[3], intAcfSumSq[3],
     intAcfTrunc[3], intAcfTruncSq[3], deltaT, fac, x, w;
  int nValAcf, k, j, n, nSet, nValInt, stepAcf;
  char *bp, buff[BUFF_LEN];
  FILE *fp;

  nValInt = 280;
  if ((fp = fopen (argv[1], "r")) == 0) {
    printf ("no file\n");
    exit (0);
  }
  while (1) {
    bp = fgets (buff, BUFF_LEN, fp);
    if (*bp == CHAR_MINUS) break;
    NameVal (deltaT);
    NameVal (nValAcf);
    NameVal (stepAcf);
  }
  AllocMem2 (sumAcf, 3, nValAcf, real);
  AllocMem2 (sumAcfSq, 3, nValAcf, real);
  for (j = 0; j < 3; j ++) {
    for (n = 0; n < nValAcf; n ++) sumAcf[j][n] = sumAcfSq[j][n] = 0.;
    intAcfSum[j] = intAcfSumSq[j] = 0.;
    intAcfTrunc[j] = intAcfTruncSq[j] = 0.;
  }
  for (j = 0; j < 3; j ++) AllocMem (valAcf[j], nValAcf, real);
  nSet = 0;
  while (1) {
    if (! (bp = fgets (buff, BUFF_LEN, fp))) break;
    if (! strncmp (bp, txtAcf, strlen (txtAcf))) {
      ++ nSet;
      for (n = 0; n < nValAcf; n ++) {
        bp = fgets (buff, BUFF_LEN, fp);
        x = strtod (bp, &bp);
        for (j = 0; j < 3; j ++) {
          w = strtod (bp, &bp);
          valAcf[j][n] = w;
          sumAcf[j][n] += w;
          sumAcfSq[j][n] += Sqr (w);
        }
      }
      bp = fgets (buff, BUFF_LEN, fp);
      bp += strlen (txtintAcf);
      for (j = 0; j < 3; j ++) {
        w = strtod (bp, &bp);
        intAcfSum[j] += w;
        intAcfSumSq[j] += Sqr (w);
      }
      for (j = 0; j < 3; j ++) {
        w = Integrate (valAcf[j], nValInt);
        intAcfTrunc[j] += w;
        intAcfTruncSq[j] += Sqr (w);
      }
    }
  }
  fclose (fp);
  printf ("%d\n", nSet);
  for (j = 0; j < 3; j ++) {
    for (n = 0; n < nValAcf; n ++) {
      sumAcf[j][n] /= nSet;
      sumAcfSq[j][n] = sqrt (sumAcfSq[j][n] / nSet - Sqr (sumAcf[j][n]));
    }
  }
  for (j = 0; j < 3; j ++) {
    intAcfSum[j] /= nSet;
    intAcfSumSq[j] = sqrt (intAcfSumSq[j] / nSet - Sqr (intAcfSum[j]));
    printf ("%9.4f %9.4f %9.4f\n", intAcfSum[j], intAcfSumSq[j],
       intAcfSumSq[j] / intAcfSum[j]);
  }
  for (j = 0; j < 3; j ++) {
    intAcfTrunc[j] /= nSet;
    intAcfTruncSq[j] = sqrt (intAcfTruncSq[j] / nSet - Sqr (intAcfTrunc[j]));
    fac = intAcfSum[j] / Integrate (sumAcf[j], nValAcf);
    intAcfTrunc[j] *= fac;
    intAcfTruncSq[j] *= fac;
  }
  printf ("%d\n", nValInt);
  for (j = 0; j < 3; j ++) {
    printf ("%9.4f %9.4f %9.4f\n", intAcfTrunc[j],
       intAcfTruncSq[j], intAcfTruncSq[j] / intAcfTrunc[j]);
  }
  for (n = 0; n < nValAcf; n ++) {
    x = n * stepAcf * deltaT;
    printf ("%8.4f", x);
    for (j = 0; j < 3; j ++)
       printf (" %8.4f %8.4f", sumAcf[j][n], sumAcfSq[j][n]);
    printf ("\n");
  }
}


real Integrate (real *f, int nf)
{
  real s;
  int i;

  s = 0.5 * (f[0] + f[nf - 1]);
  for (i = 1; i < nf - 1; i ++) s += f[i];
  return (s);
}


#include "in_errexit.c"

