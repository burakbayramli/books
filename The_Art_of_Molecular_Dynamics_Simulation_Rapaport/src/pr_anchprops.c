
/* [[pr_anchprops - polymer analysis]] */


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

char *txtChn = "chain props:";

int main (int argc, char **argv)
{
  Prop vProp[5];
  real bSum[5];
  int chainLen, j, nData, nDataGroup, nDataSkip, nv;
  char *bp, buff[BUFF_LEN];
  FILE *fp;

  if ((fp = fopen (argv[1], "r")) == 0) {
    printf ("no file\n");
    exit (0);
  }
  while (1) {
    bp = fgets (buff, BUFF_LEN, fp);
    if (*bp == CHAR_MINUS) break;
    NameVal (chainLen);
  }
  nDataGroup = 30;
  nDataSkip = 10;
  nData = 0;
  for (j = 0; j < 5; j ++) {
    PropZero (vProp[j]);
    bSum[j] = 0.;
  }
  nv = 0.;
  while (1) {
    if (! (bp = fgets (buff, BUFF_LEN, fp))) break;
    if (! strncmp (bp, txtChn, strlen (txtChn))) {
      if (++ nData > nDataSkip) {
        bp += strlen (txtChn);
        for (j = 0; j < 5; j ++) bSum[j] += strtod (bp, &bp);
        if ((nData - nDataSkip) % nDataGroup == 0) {
          for (j = 0; j < 5; j ++) {
            vProp[j].val = bSum[j] / nDataGroup;
            PropAccum (vProp[j]);
            bSum[j] = 0.;
          }
          ++ nv;
        }
      }
    }
  }
  printf ("%d %d %s\n", chainLen, nData - nDataSkip, txtChn);
  for (j = 0; j < 5; j ++) {
    PropAvg (vProp[j], nv);
    printf (" %9.4f %9.4f\n", PropEst (vProp[j]));
  }
}

