// Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

#include "mex.h"
#include <cmath>

#define p(i,j) p[(i)+np*(j)]
#define pv(i,j) pv[(i)+nvs*(j)]
#define ds(i,j) ds[(i)+np*(j)]

template<class T> inline T sqr(T x) { return x*x; }
template<class T> inline T dot2(T *a,T *b) { return a[0]*b[0]+a[1]*b[1]; }
template<class T> inline T length(T x,T y) { return sqrt(sqr(x)+sqr(y)); }

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  int np=mxGetM(prhs[0]);
  int nvs=mxGetM(prhs[1]);
  double *p=mxGetPr(prhs[0]);
  double *pv=mxGetPr(prhs[1]);

  plhs[0]=mxCreateDoubleMatrix(np,nvs-1,mxREAL);
  double *ds=mxGetPr(plhs[0]);

  for (int iv=0; iv<nvs-1; iv++) {
    for (int ip=0; ip<np; ip++) {
      double v[2]={pv(iv+1,0)-pv(iv,0),
                   pv(iv+1,1)-pv(iv,1)};
      double w[2]={p(ip,0)-pv(iv,0),
                   p(ip,1)-pv(iv,1)};

      double c1=dot2(v,w);
      double c2=dot2(v,v);

      if (c1<=0)
        ds(ip,iv)=length(p(ip,0)-pv(iv,0),p(ip,1)-pv(iv,1));
      else if (c1>=c2)
        ds(ip,iv)=length(p(ip,0)-pv(iv+1,0),p(ip,1)-pv(iv+1,1));
      else
        ds(ip,iv)=length(p(ip,0)-(pv(iv,0)+c1/c2*v[0]),
                         p(ip,1)-(pv(iv,1)+c1/c2*v[1]));
    }
  }
}
