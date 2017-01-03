#ifndef GRIDLOOP_C_H
#define GRIDLOOP_C_H

typedef double (*Fxy)(double x, double y);

void gridloop_C(double **a, double *xcoor, double *ycoor, 
		int nx, int ny, Fxy func1);
#endif
