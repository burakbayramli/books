
/* [[pr_anblockavg - block-averaged variance]] */


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

int main ()
{
  Prop w;
  real wVal[20000];
  int bSize, n, nv, nvh;
  char *bp, buff[BUFF_LEN];

  nv = 0.;
  while (1) {
    if (! (bp = fgets (buff, BUFF_LEN, stdin))) break;
    wVal[nv] = strtod (bp, &bp);
    ++ nv;
  }
  printf ("%d\n", nv);
  nvh = nv;
  bSize = 1;
  while (nvh > 1) {
    PropZero (w);
    for (n = 0; n < nvh; n ++) {
      w.val = wVal[n];
      PropAccum (w);
    }
    PropAvg (w, nvh);
    if (bSize == 1) printf ("av stdev %.4f %.4f\n", PropEst (w));
    printf ("%6d %6d %.4f\n", nvh, bSize, w.sum2 / sqrt (nvh - 1.));
    for (n = 0; n < nvh / 2; n ++)
       wVal[n] = 0.5 * (wVal[2 * n] + wVal[2 * n + 1]);
    nvh /= 2;
    bSize *= 2;
  }
}

