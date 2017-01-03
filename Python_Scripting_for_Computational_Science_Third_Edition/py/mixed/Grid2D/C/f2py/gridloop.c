typedef double (*Fxy)(double x, double y);

#define index(a, i, j) a[j*ny + i]

void gridloop2(double *a, double *xcoor, double *ycoor,
	       int nx, int ny, Fxy func1)
{
  int i, j;
  for (i=0; i<nx; i++) {
    for (j=0; j<ny; j++) {
      index(a, i, j) = func1(xcoor[i], ycoor[j]);
    }
  }
}
