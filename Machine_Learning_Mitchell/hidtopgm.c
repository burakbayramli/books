/*
   hidtopgm - scale hidden-unit weights to pixel values
 */

#include <stdio.h>
#include <math.h>
#include "pgmimage.h"
#include "backprop.h"

#define MAXROWS 256
#define MAXCOLS 256

double tmpimg;

main(argc, argv)
int argc;
char *argv[];
{
  BPNN *net;
  IMAGE *img;
  int nr, nc,  i, j, k, h, pxl;
  double maxwt, minwt, range;

  if (argc < 6) {
    fprintf(stderr, "usage:  %s net-file image-file x y hidden-unit-num\n",
	    argv[0]);
    exit(1);
  }

  if ((net = bpnn_read(argv[1])) == NULL) {
    fprintf(stderr, "%s:  can't read net-file '%s'\n", argv[0], argv[1]);
    exit(1);
  }
      
  nc = atoi(argv[3]);
  nr = atoi(argv[4]);
  h = atoi(argv[5]);
  if ((img = img_creat(argv[2], nr, nc)) == NULL) {
    fprintf(stderr, "%s:  can't create image-file '%s'\n", argv[0], argv[2]);
    exit(1);
  }
      
  /* first get min and max wts */
  k = 0;
  maxwt = -1e6;
  minwt = 1e6;
  for (i = 0; i < nr; i++) {
    for (j = 0; j < nc; j++) {
      if (net->input_weights[k][h] > maxwt)
	maxwt = net->input_weights[k][h];
      if (net->input_weights[k][h] < minwt)
	minwt = net->input_weights[k][h];
      k++;
    }
  }
  range = maxwt - minwt;

  /* now scale values */
  k = 0;
  for (i = 0; i < nr; i++) {
    for (j = 0; j < nc; j++) {
      tmpimg = net->input_weights[k][h];
      pxl = ((tmpimg-minwt)/range) * 255.0;
      img_setpixel(img, i, j, pxl);
      k++;
    }
  }

  img_write(img, argv[2]);
  exit(0);
}
