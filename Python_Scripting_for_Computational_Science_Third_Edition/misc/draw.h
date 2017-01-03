#ifndef __draw_h_included
#define __draw_h_included

#ifdef __cplusplus
extern "C" {
#endif

/* functions for drawing random numbers */
extern void setSeed (int seed);
/* draw random number in [0,1]: */
extern double draw01 ();
/* draw from the normal distribution: */
extern double gauss (double mean, double stdev);

#ifdef __cplusplus
}
#endif
#endif

