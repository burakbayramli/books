// Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

#include "mex.h"
#include <algorithm>
#include <cmath>
#include <cstring>

using namespace std;
template<class T> inline T sqr(T x) { return x*x; }
typedef ptrdiff_t szint;

void roots(double *pol,double *rr,double *ri,szint n);
double dellipsoid(double x0,double y0,double z0,double a,double b,double c);

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  int np=mxGetM(prhs[0]);
  double *p=mxGetPr(prhs[0]);
  double *axes=mxGetPr(prhs[1]);
  plhs[0]=mxCreateDoubleMatrix(np,1,mxREAL);
  double *d=mxGetPr(plhs[0]);

  for (int ip=0; ip<np; ip++) {
    d[ip]=dellipsoid(p[ip],p[ip+np],p[ip+2*np],axes[0],axes[1],axes[2]);
  }
}

double dellipsoid(double x0,double y0,double z0,double a,double b,double c)
{
  // Begin Maple Generated
  double t1,t2,t3,t5,t6,t7,t8,t9,t10,t11,t14,t15,
         t16,t17,t18,t20,t22,t24,t41,t42,t60;

  t1=a*a; t2=b*b; t3=c*c; t5=x0*x0; t6=t1*t5; t7=t1*t1; t8=t1*t2; t9=4.0*t8;
  t10=t2*t2; t11=t1+t2; t14=t3*t3; t15=y0*y0; t16=t2*t15; t17=z0*z0;
  t18=t3*t17; t20=t7*t2; t22=t1*t10; t24=t7+t9+t10; t41=t7*t10; t42=t20+t22;
  t60=t41+4.0*t42*t3+t24*t14-t18*t7-4.0*t18*t8-t18*t10-t16*t7-
      4.0*t16*t1*t3-t16*t14-t6*t10-4.0*t6*t2*t3-t6*t14;
  double pol[7]= {
    1.0,
    2.0*t1+2.0*t2+2.0*t3,
    -t6+t7+t9+t10+4.0*t11*t3+t14-t16-t18,
    2.0*t20+2.0*t22+2.0*t24*t3+2.0*t11*t14-2.0*t16*t1-2.0*t16*t3-2.0*t6*t2-2.0*t6*t3-2.0*t18*t1-2.0*t18*t2,
    t60,
    2.0*t41*t3+2.0*t42*t14-2.0*t18*t20-2.0*t18*t22-2.0*t16*t7*t3-2.0*t16*t1*t14-2.0*t6*t10*t3-2.0*t6*t2*t14,
    t41*t14-t6*t10*t14-t18*t41-t16*t7*t14
  };
  // End Maple Generated

  double rr[6],ri[6];
  roots(pol,rr,ri,6);

  double t=-HUGE_VAL;
  for (int i=0; i<6; i++)
    t=max(t,rr[i]);

  double x=sqr(a)*x0/(t+sqr(a));
  double y=sqr(b)*y0/(t+sqr(b));
  double z=sqr(c)*z0/(t+sqr(c));
  double d=t*sqrt(sqr(x)/sqr(sqr(a))+sqr(y)/sqr(sqr(b))+sqr(z)/sqr(sqr(c)));

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
