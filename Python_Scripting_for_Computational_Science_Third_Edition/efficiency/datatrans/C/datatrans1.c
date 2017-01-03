#include <stdlib.h>
#include <stdio.h>
#include <math.h>

double myfunc(double y)
{
  if (y >= 0.0) {
    return pow(y,5.0)*exp(-y);
  } else {
    return 0.0;
  }
}

int main (int argc, char* argv[])
{
  FILE *ifile;  /* input  file */
  FILE *ofile;  /* outout file */
  double x, y; 
  char *infilename;
  char *outfilename;
  int  n;
  int  ok;

  /* abort if there are too few command-line arguments */
  if (argc < 3) {
    printf("Usage: %s infile outfile\n", argv[0]);  exit(1);
  } else {
    infilename = argv[1]; outfilename = argv[2];
  }
  printf("%s: converting %s to %s\n",argv[0],infilename,outfilename);
  ifile = fopen( infilename, "r"); /* open for reading */
  ofile = fopen(outfilename, "w"); /* open for writing */

  ok = 1;  /* boolean variable for not end of file */
  while (ok) {
    n = fscanf(ifile, "%lf%lf", &x, &y); /* read x and y */
    if (n == 2) {
      /* successful read in fscanf: */
      /* printf("%g %12.5e\n", x, y); */
      y = myfunc(y);
      fprintf(ofile, "%g %12.5e\n", x, y);
    } else {
      /* no more numbers */ ok = 0;
    }
  }
  fclose(ifile);  fclose(ofile);
  return 0;
}





