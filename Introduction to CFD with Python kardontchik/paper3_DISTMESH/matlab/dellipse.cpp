// Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

#include "mex.h"
#include <algorithm>
#include <cmath>
#include <cstring>

using namespace std;
template<class T> inline T sqr(T x) { return x*x; }
typedef ptrdiff_t szint;

void roots(double *pol,double *rr,double *ri,szint n);
double dellipse(double x0,double y0,double a,double b);

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  szint np=mxGetM(prhs[0]);
  double *p=mxGetPr(prhs[0]);
  double *axes=mxGetPr(prhs[1]);
  plhs[0]=mxCreateDoubleMatrix(np,1,mxREAL);
  double *d=mxGetPr(plhs[0]);

  for (int ip=0; ip<np; ip++) {
    d[ip]=dellipse(p[ip],p[ip+np],axes[0],axes[1]);
  }
}

double dellipse(double x0,double y0,double a,double b)
{
  double t1,t2,t4,t6,t8,t9,t11,t15,t16;
  t1=a*a; t2=b*b; t4=y0*y0; t6=x0*x0; t8=t1*t1;  
  t9=t2*t1; t11=t2*t2; t15=t8*t2; t16=t11*t1;
  double pol[5]={ 1.0, 2.0*t1+2.0*t2, -t2*t4-t1*t6+t8+4.0*t9+t11,
                  -2.0*t9*t6-2.0*t9*t4+2.0*t15+2.0*t16,
                  -t16*t6+t8*t11-t15*t4 };

  double rr[4],ri[4];
  roots(pol,rr,ri,4);

  double t=-HUGE_VAL;
  for (int i=0; i<4; i++)
    t=max(t,rr[i]);

  double x=sqr(a)*x0/(t+sqr(a));
  double y=sqr(b)*y0/(t+sqr(b));
  double d=t*sqrt(sqr(x)/sqr(sqr(a))+sqr(y)/sqr(sqr(b)));

  return d;
}

extern "C" { void dhseqr_(char*,char*,szint*,szint*,szint*,double*,szint*,double*,
                         double*,void*,szint*,double*,void*,szint*); }

void roots(double *pol,double *rr,double *ri,szint n)
{
  double *H,*work;
  szint o=1;
  szint info;
  char chE='E',chN='N';
  H=(double*)mxCalloc(n*n,sizeof(double));
  work=(double*)mxCalloc(n,sizeof(double));

  memset(H,0,n*n*sizeof(double));
  for (int i=0; i<n-1; i++)
    H[1+(n+1)*i]=1.0;
  for (int i=0; i<n; i++)
    H[n*i]=-pol[i+1]/pol[0];
  
  dhseqr_(&chE,&chN,&n,&o,&n,H,&n,rr,ri,0,&n,work,&n,&info);

  mxFree(work); mxFree(H);
  if (info!=0)
    mexErrMsgTxt("Roots not found.");
}
