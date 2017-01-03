#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

void NR::dfpmin(Vec_IO_DP &p, const DP gtol, int &iter, DP &fret,
	DP func(Vec_I_DP &), void dfunc(Vec_I_DP &, Vec_O_DP &))
{
	const int ITMAX=200;
	const DP EPS=numeric_limits<DP>::epsilon();
	const DP TOLX=4*EPS,STPMX=100.0;
	bool check;
	int i,its,j;
	DP den,fac,fad,fae,fp,stpmax,sum=0.0,sumdg,sumxi,temp,test;

	int n=p.size();
	Vec_DP dg(n),g(n),hdg(n),pnew(n),xi(n);
	Mat_DP hessin(n,n);
	fp=func(p);
	dfunc(p,g);
	for (i=0;i<n;i++) {
		for (j=0;j<n;j++) hessin[i][j]=0.0;
		hessin[i][i]=1.0;
		xi[i] = -g[i];
		sum += p[i]*p[i];
	}
	stpmax=STPMX*MAX(sqrt(sum),DP(n));
	for (its=0;its<ITMAX;its++) {
		iter=its;
		lnsrch(p,fp,g,xi,pnew,fret,stpmax,check,func);
		fp=fret;
		for (i=0;i<n;i++) {
			xi[i]=pnew[i]-p[i];
			p[i]=pnew[i];
		}
		test=0.0;
		for (i=0;i<n;i++) {
			temp=fabs(xi[i])/MAX(fabs(p[i]),1.0);
			if (temp > test) test=temp;
		}
		if (test < TOLX)
			return;
		for (i=0;i<n;i++) dg[i]=g[i];
		dfunc(p,g);
		test=0.0;
		den=MAX(fret,1.0);
		for (i=0;i<n;i++) {
			temp=fabs(g[i])*MAX(fabs(p[i]),1.0)/den;
			if (temp > test) test=temp;
		}
		if (test < gtol)
			return;
		for (i=0;i<n;i++) dg[i]=g[i]-dg[i];
		for (i=0;i<n;i++) {
			hdg[i]=0.0;
			for (j=0;j<n;j++) hdg[i] += hessin[i][j]*dg[j];
		}
		fac=fae=sumdg=sumxi=0.0;
		for (i=0;i<n;i++) {
			fac += dg[i]*xi[i];
			fae += dg[i]*hdg[i];
			sumdg += SQR(dg[i]);
			sumxi += SQR(xi[i]);
		}
		if (fac > sqrt(EPS*sumdg*sumxi)) {
			fac=1.0/fac;
			fad=1.0/fae;
			for (i=0;i<n;i++) dg[i]=fac*xi[i]-fad*hdg[i];
			for (i=0;i<n;i++) {
				for (j=i;j<n;j++) {
					hessin[i][j] += fac*xi[i]*xi[j]
						-fad*hdg[i]*hdg[j]+fae*dg[i]*dg[j];
					hessin[j][i]=hessin[i][j];
				}
			}
		}
		for (i=0;i<n;i++) {
			xi[i]=0.0;
			for (j=0;j<n;j++) xi[i] -= hessin[i][j]*g[j];
		}
	}
	nrerror("too many iterations in dfpmin");
}
