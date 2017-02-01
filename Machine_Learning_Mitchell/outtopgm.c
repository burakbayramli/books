/*
   outtopgm - scale unit output weights to pixel values. 

 Example: Following creates .pgm file to display weights for output
          unit 2, from hidden layer containing 4 hidden units (plus threshold wt)
 outtopgm pose.net pose.pgm 5 1 2

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
  int nr, nc,  i, j, k, out, pxl;
  double maxwt, minwt, range;

  if (argc < 6) {
    fprintf(stderr, "usage:  %s net-file image-file x y output-unit-num\n",
	    argv[0]);
    exit(1);
  }

  if ((net = bpnn_read(argv[1])) == NULL) {
    fprintf(stderr, "%s:  can't read net-file '%s'\n", argv[0], argv[1]);
    exit(1);
  }
      
  nc = atoi(argv[3]);
  nr = atoi(argv[4]);
  out = atoi(argv[5]);
  if ((img = img_creat(argv[2], 2*nr, 2*nc)) == NULL) {
    fprintf(stderr, "%s:  can't create image-file '%s'\n", argv[0], argv[2]);
    exit(1);
  }
      
  /* let the user in on the weight values */
  fprintf(stdout, "Output unit %s:\n Weight values: \n", argv[5]);  
  for (i = 0; i < nc; i++) {
    printf("i=%d\n", i);
    fprintf(stdout, "  %g\n", net->hidden_weights[i][out]);
  }
  /* first get min and max wts */
  k = 0;
  maxwt = -1e6;
  minwt = 1e6;
  for (i = 0; i < nr; i++) {
    for (j = 0; j < nc; j++) {
      if (net->hidden_weights[k][out] > maxwt)
	maxwt = net->hidden_weights[k][out];
      if (net->hidden_weights[k][out] < minwt)
	minwt = net->hidden_weights[k][out];
      k++;
    }
  }
  range = maxwt - minwt;
  /* now scale values */
  k = 0;
  for (i = 0; i < nr; i++) {
    for (j = 0; j < nc; j++) {
      tmpimg = net->hidden_weights[k][out];
      pxl = ((tmpimg-minwt)/range) * 255.0;
      /* because of xv problem with single-line images, make image double the
         number of rows and columns */
      img_setpixel(img, 2*i, 2*j, pxl);
      img_setpixel(img, 1+2*i, 1+2*j, pxl);
      img_setpixel(img, 2*i, 1+2*j, pxl);
      img_setpixel(img, 1+2*i, 2*j, pxl);
      k++;
    }
  }

  img_write(img, argv[2]);
  exit(0);
}
