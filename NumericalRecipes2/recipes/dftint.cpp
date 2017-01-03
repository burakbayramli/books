#include <cmath>
#include "nr.h"
using namespace std;

void NR::dftint(DP func(const DP), const DP a, const DP b, const DP w,
	DP &cosint, DP &sinint)
{
	static int init=0;
	static DP (*funcold)(const DP);
	static DP aold = -1.e30,bold = -1.e30,delta;
	const int M=64,NDFT=1024,MPOL=6;
	const DP TWOPI=6.283185307179586476;
	int j,nn;
	DP c,cdft,cerr,corfac,corim,corre,en,s,sdft,serr;
	static Vec_DP data(NDFT),endpts(8);
	Vec_DP cpol(MPOL),spol(MPOL),xpol(MPOL);

	if (init != 1 || a != aold || b != bold || func != funcold) {
		init=1;
		aold=a;
		bold=b;
		funcold=func;
		delta=(b-a)/M;
		for (j=0;j<M+1;j++)
			data[j]=func(a+j*delta);
		for (j=M+1;j<NDFT;j++)
			data[j]=0.0;
		for (j=0;j<4;j++) {
			endpts[j]=data[j];
			endpts[j+4]=data[M-3+j];
		}
		realft(data,1);
		data[1]=0.0;
	}
	en=w*delta*NDFT/TWOPI+1.0;
	nn=MIN(MAX(int(en-0.5*MPOL+1.0),1),NDFT/2-MPOL+1);
	for (j=0;j<MPOL;j++,nn++) {
		cpol[j]=data[2*nn-2];
		spol[j]=data[2*nn-1];
		xpol[j]=nn;
	}
	polint(xpol,cpol,en,cdft,cerr);
	polint(xpol,spol,en,sdft,serr);
	dftcor(w,delta,a,b,endpts,corre,corim,corfac);
	cdft *= corfac;
	sdft *= corfac;
	cdft += corre;
	sdft += corim;
	c=delta*cos(w*a);
	s=delta*sin(w*a);
	cosint=c*cdft-s*sdft;
	sinint=s*cdft+c*sdft;
}
