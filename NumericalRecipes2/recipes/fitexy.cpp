#include <cmath>
#include "nr.h"
using namespace std;

Vec_DP *xx_p,*yy_p,*sx_p,*sy_p,*ww_p;
DP aa,offs;

void NR::fitexy(Vec_I_DP &x, Vec_I_DP &y, Vec_I_DP &sigx, Vec_I_DP &sigy,
	DP &a, DP &b, DP &siga, DP &sigb, DP &chi2, DP &q)
{
	int j;
	const DP POTN=1.571000,BIG=1.0e30,ACC=1.0e-3;
	const DP PI=3.141592653589793238;
	DP amx,amn,varx,vary,ang[7],ch[7],scale,bmn,bmx,d1,d2,r2,
		dum1,dum2,dum3,dum4,dum5;

	int ndat=x.size();
	xx_p=new Vec_DP(ndat);
	yy_p=new Vec_DP(ndat);
	sx_p=new Vec_DP(ndat);
	sy_p=new Vec_DP(ndat);
	ww_p=new Vec_DP(ndat);
	Vec_DP &xx=*xx_p, &yy=*yy_p;
	Vec_DP &sx=*sx_p, &sy=*sy_p, &ww=*ww_p;
	avevar(x,dum1,varx);
	avevar(y,dum1,vary);
	scale=sqrt(varx/vary);
	for (j=0;j<ndat;j++) {
		xx[j]=x[j];
		yy[j]=y[j]*scale;
		sx[j]=sigx[j];
		sy[j]=sigy[j]*scale;
		ww[j]=sqrt(SQR(sx[j])+SQR(sy[j]));
	}
	fit(xx,yy,ww,true,dum1,b,dum2,dum3,dum4,dum5);
	offs=ang[0]=0.0;
	ang[1]=atan(b);
	ang[3]=0.0;
	ang[4]=ang[1];
	ang[5]=POTN;
	for (j=3;j<6;j++) ch[j]=chixy(ang[j]);
	mnbrak(ang[0],ang[1],ang[2],ch[0],ch[1],ch[2],chixy);
	chi2=brent(ang[0],ang[1],ang[2],chixy,ACC,b);
	chi2=chixy(b);
	a=aa;
	q=gammq(0.5*(ndat-2),chi2*0.5);
	r2=0.0;
	for (j=0;j<ndat;j++) r2 += ww[j];
	r2=1.0/r2;
	bmx=BIG;
	bmn=BIG;
	offs=chi2+1.0;
	for (j=0;j<6;j++) {
		if (ch[j] > offs) {
			d1=fabs(ang[j]-b);
			while (d1 >= PI) d1 -= PI;
			d2=PI-d1;
			if (ang[j] < b)
				SWAP(d1,d2);
			if (d1 < bmx) bmx=d1;
			if (d2 < bmn) bmn=d2;
		}
	}
	if (bmx < BIG) {
		bmx=zbrent(chixy,b,b+bmx,ACC)-b;
		amx=aa-a;
		bmn=zbrent(chixy,b,b-bmn,ACC)-b;
		amn=aa-a;
		sigb=sqrt(0.5*(bmx*bmx+bmn*bmn))/(scale*SQR(cos(b)));
		siga=sqrt(0.5*(amx*amx+amn*amn)+r2)/scale;
	} else sigb=siga=BIG;
	a /= scale;
	b=tan(b)/scale;
	delete ww_p; delete sy_p; delete sx_p; delete yy_p; delete xx_p;
}
