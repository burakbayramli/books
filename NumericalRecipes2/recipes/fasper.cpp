#include <cmath>
#include "nr.h"
using namespace std;

void NR::fasper(Vec_I_DP &x, Vec_I_DP &y, const DP ofac, const DP hifac,
	Vec_O_DP &wk1, Vec_O_DP &wk2, int &nout, int &jmax, DP &prob)
{
	const int MACC=4;
	int j,k,ndim,nfreq,nfreqt;
	DP ave,ck,ckk,cterm,cwt,den,df,effm,expy,fac,fndim,hc2wt,hs2wt,
		hypo,pmax,sterm,swt,var,xdif,xmax,xmin;

	int n=x.size();
	int nwk=wk1.size();
	nout=0.5*ofac*hifac*n;
	nfreqt=ofac*hifac*n*MACC;
	nfreq=64;
	while (nfreq < nfreqt) nfreq <<= 1;
	ndim=nfreq << 1;
	if (ndim > nwk) nrerror("workspaces too small in fasper");
	avevar(y,ave,var);
	if (var == 0.0) nrerror("zero variance in fasper");
	xmin=x[0];
	xmax=xmin;
	for (j=1;j<n;j++) {
		if (x[j] < xmin) xmin=x[j];
		if (x[j] > xmax) xmax=x[j];
	}
	xdif=xmax-xmin;
	Vec_DP wk1_t(0.0,ndim);
	Vec_DP wk2_t(0.0,ndim);
	fac=ndim/(xdif*ofac);
	fndim=ndim;
	for (j=0;j<n;j++) {
		ck=fmod((x[j]-xmin)*fac,fndim);
		ckk=2.0*(ck++);
		ckk=fmod(ckk,fndim);
		++ckk;
		spread(y[j]-ave,wk1_t,ck,MACC);
		spread(1.0,wk2_t,ckk,MACC);
	}
	realft(wk1_t,1);
	realft(wk2_t,1);
	df=1.0/(xdif*ofac);
	pmax = -1.0;
	for (k=2,j=0;j<nout;j++,k+=2) {
		hypo=sqrt(wk2_t[k]*wk2_t[k]+wk2_t[k+1]*wk2_t[k+1]);
		hc2wt=0.5*wk2_t[k]/hypo;
		hs2wt=0.5*wk2_t[k+1]/hypo;
		cwt=sqrt(0.5+hc2wt);
		swt=SIGN(sqrt(0.5-hc2wt),hs2wt);
		den=0.5*n+hc2wt*wk2_t[k]+hs2wt*wk2_t[k+1];
		cterm=SQR(cwt*wk1_t[k]+swt*wk1_t[k+1])/den;
		sterm=SQR(cwt*wk1_t[k+1]-swt*wk1_t[k])/(n-den);
		wk1[j]=(j+1)*df;
		wk2[j]=(cterm+sterm)/(2.0*var);
		if (wk2[j] > pmax) pmax=wk2[jmax=j];
	}
	expy=exp(-pmax);
	effm=2.0*nout/ofac;
	prob=effm*expy;
	if (prob > 0.01) prob=1.0-pow(1.0-expy,effm);
}
