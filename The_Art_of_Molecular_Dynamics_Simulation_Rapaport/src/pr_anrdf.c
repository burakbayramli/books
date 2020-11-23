
/* [[pr_anrdf - RDF analysis]] */


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
#define NRDF  3

char *txtRdf = "rdf";

int main (int argc, char **argv)
{
  Prop **rdf;
  real *rVal;
  int j, k, n, nData, nDataSkip, nv, sizeHistRdf;
  char buff[BUFF_LEN], *bp;
  FILE *fp;

  if ((fp = fopen (argv[1], "r")) == 0) {
    printf ("no file\n");
    exit (0);
  }
  while (1) {
    bp = fgets (buff, BUFF_LEN, fp);
    if (*bp == CHAR_MINUS) break;
    NameVal (sizeHistRdf);
  }
  AllocMem2 (rdf, NRDF, sizeHistRdf, Prop);
  for (j = 0; j < NRDF; j ++) {
    for (n = 0; n < sizeHistRdf; n ++) PropZero (rdf[j][n]);
  }
  AllocMem (rVal, sizeHistRdf, real);
  nDataSkip = 0;
  nv = 0;
  nData = 0;
  while (1) {
    if (! (bp = fgets (buff, BUFF_LEN, fp))) break;
    if (! strncmp (bp, txtRdf, strlen (txtRdf))) {
      if (++ nData > nDataSkip) {
        ++ nv;
        for (n = 0; n < sizeHistRdf; n ++) {
          bp = fgets (buff, BUFF_LEN, fp);
          rVal[n] = strtod (bp, &bp);
          for (j = 0; j < NRDF; j ++) {
            rdf[j][n].val = strtod (bp, &bp);
            PropAccum (rdf[j][n]);
          }
        }
      }
    }
  }
  fclose (fp);
  printf ("%d\n", nv);
  for (j = 0; j < NRDF; j ++) {
    for (n = 0; n < sizeHistRdf; n ++) PropAvg (rdf[j][n], nv);
  }
  for (n = 0; n < sizeHistRdf; n ++) {
    printf ("%9.4f", rVal[n]);
    for (j = 0; j < NRDF; j ++) printf (" %9.4f %9.4f", PropEst (rdf[j][n]));
    printf ("\n");
  }
}

